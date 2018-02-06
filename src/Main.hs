{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Rel8
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource         (runResourceT)
import           Data.Default
import           Data.Foldable
import           Data.Functor
import           Data.Hashable
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable

import           Data.Fixed
import           Data.Int
import           Data.Scientific                      as Sci
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.IO                    as TL

import           Data.Time
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified Database.PostgreSQL.Simple           as PGS
import           Database.PostgreSQL.Simple.FromField (FieldParser,
                                                       FromField (..))
import qualified Opaleye                              as O
import           Rel8                                 hiding (max)
import qualified Rel8.Internal                        as RI 
import qualified Rel8.IO                              as R
import           Streaming
import qualified Streaming.Prelude                    as S

import           Prelude                              hiding (Int)

import qualified Data.Attoparsec.ByteString           as ABS
import qualified Data.Attoparsec.Text                 as AT

import qualified Data.Attoparsec.ByteString           as A
import qualified Data.Attoparsec.ByteString.Char8     as AQ
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Streaming.Char8      as Q
import qualified Data.Text.Encoding                   as TE

import           System.IO                            (Handle, IOMode (..),
                                                       withFile)

import           Control.Arrow                        ((>>>))
import           Data.Word
import qualified Debug.Trace                          as Trace

instance DBEq Integer
instance DBEq Day

----------------------------------------------------------------------

newtype EaseFcr = EaseFcr Double
  deriving (Show, Ord, Eq, DBType, FromField)

unEaseFcr :: EaseFcr -> Double
unEaseFcr (EaseFcr ef) = ef

newtype Quality = Quality Int16
  deriving (Show, DBType, FromField)

unQuality :: Quality -> Int16
unQuality (Quality q) = q

newtype DayInterval = DayInterval Int64
  deriving (Show, DBType, FromField)

unDayInterval :: DayInterval -> Int64
unDayInterval (DayInterval i) = i

----------------------------------------------------------------------

mkQuality :: Int16 -> Quality
mkQuality n | n < 0 || n > 5 = error "quality out of bounds"
            | otherwise      = Quality n

mkEaseFcr :: Double -> EaseFcr
mkEaseFcr n | n < 1.2 || n > 3.0 = error "easefcr out of bounds"
            | otherwise          = EaseFcr n

defaultEaseFcr :: EaseFcr
defaultEaseFcr = EaseFcr 2.5

defaultDayIntervalBase :: Int64
defaultDayIntervalBase = 1

defaultDayInterval :: Int64
defaultDayInterval = 2

lowerThresholdEaseFcr :: EaseFcr
lowerThresholdEaseFcr = EaseFcr 1.3

addDayIntervalToUTC :: DayInterval -> Day -> Day
addDayIntervalToUTC (DayInterval s) = addDays (fromIntegral s)

----------------------------------------------------------------------

-- | Calculate the interval till the next repetition based
-- on the quality of a recall event.
nextRepIval :: EaseFcr -> Quality -> DayInterval
nextRepIval (EaseFcr ef) (Quality q) = DayInterval (round (go ef q))
 where
  go _  1 = fromIntegral defaultDayIntervalBase
  go _  2 = fromIntegral defaultDayInterval
  go ef n = fromIntegral defaultDayInterval * ef ** (fromIntegral n - 2)
  -- go item n = go item (n - 1) * (item ^. easeFcr . to unEaseFcr)

-- | Modify the E-factor based on the quality of a single recall.
updatedEaseFcr :: EaseFcr -> Quality -> EaseFcr
updatedEaseFcr (EaseFcr ef) (Quality (fromIntegral -> q)) = max
  lowerThresholdEaseFcr
  updated
 where
  delta   = 0.1 - 0.04 * (5 - q) * (9 - q)
  updated = EaseFcr (ef + delta)

----------------------------------------------------------------------

newtype CardId = CardId Int64
  deriving (Show, DBType, DBEq, FromField)

makePrisms ''CardId

data Card f = Card
  { _cardId   :: C f "id"          'HasDefault CardId
  , _easeFcr  :: C f "ease_factor" 'NoDefault  EaseFcr
  , _repsDone :: C f "reps_done"   'NoDefault  Int64
  , _nextDue  :: C f "next_due"    'NoDefault  Day
  , _interval :: C f "interval"    'NoDefault  DayInterval
  , _front    :: C f "front"       'NoDefault  Text
  , _back     :: C f "back"        'NoDefault  Text
  } deriving (Generic)

makeLenses ''Card

type CardQ = Card QueryResult
type CardE = Card Expr

instance BaseTable Card where tableName = "card"

instance (e ~ Expr, q ~ QueryResult) => Table (Card e) (Card q)

deriving instance (q ~ QueryResult) => Show (Card q)

updateCard :: Day -> CardQ -> Quality -> CardQ
updateCard today c@Card {..} q = c { _repsDone = _repsDone'
                                  , _nextDue  = _nextDue'
                                  , _interval = _interval'
                                  , _easeFcr  = _easeFcr'
                                  }
 where
  bad        = unQuality q < 3
  repeatToday = unQuality q < 4

  _interval' = nextRepIval _easeFcr q
  _nextDue'  
    | repeatToday = today
    | otherwise   = addDayIntervalToUTC _interval' today

  _repsDone' | bad       = 1
             | otherwise = _repsDone + 1

  _easeFcr' | bad       = _easeFcr
            | otherwise = updatedEaseFcr _easeFcr q

connInfo :: PGS.ConnectInfo
connInfo = PGS.defaultConnectInfo { PGS.connectDatabase = "procon"
                                  , PGS.connectUser     = "frob"
                                  , PGS.connectPassword = "frob"
                                  }

runQuery :: (PGS.Connection -> IO b) -> IO b
runQuery foo = PGS.connect connInfo >>= foo

runSelect :: Show a => Table rows a => Query rows -> Connection -> IO ()
runSelect q conn =
  runResourceT (S.mapM_ (lift . print) (R.select (R.stream conn) q))

parseQuality :: AT.Parser Quality
parseQuality = mkQuality <$> AT.decimal

----------------------------------------------------------------------
-- Parsing and loading cards from disk
----------------------------------------------------------------------

data Section = Front | Back
  deriving (Show, Eq)

data ParsedElement a
  = StartCard
  | StartSection Section
  | BlankLine
  | LineOfText !a
  deriving (Show, Functor, Foldable, Eq)

type B = BS.ByteString
type P = ParsedElement B

data FoldState
  = Initial
  | Await !Section
  | Inside !Section !Int
  deriving (Show)

parseLine :: A.Parser (ParsedElement BS.ByteString)
parseLine =
  (BlankLine <$ AQ.endOfLine)
    <|> A.string "%%%"
    *>  A.skipMany1 AQ.space
    *>  (   (StartSection Front <$ A.string "FRONT" <* AQ.endOfLine)
        <|> (StartSection Back <$ A.string "BACK" <* AQ.endOfLine)
        <|> (StartCard <$ A.string "CARD_START" <* AQ.endOfLine)
        )
    <|> (LineOfText <$> A.takeTill AQ.isEndOfLine <* AQ.endOfLine)

streamedTokens :: Handle -> Stream (Of P) IO ()
streamedTokens =
  Q.fromHandle >>> A.parsed parseLine -- preprocess
                                      >>> void -- discard any pending input

-- | Stream card (front, back) pairs from a file handle.
streamCardsFrom :: Handle -> Stream (Of (B, B)) IO ()
streamCardsFrom = streamedTokens >>> S.split StartCard >>> S.mapped
  (S.fold go' (Initial, ("", "")) snd)
 where
  go' :: (FoldState, (B, B)) -> P -> (FoldState, (B, B))
  go' (st, (q, a)) p = let (st', q', a') = go st q a p in (st', (q', a'))

  go :: FoldState -> B -> B -> P -> (FoldState, B, B)
  -- Skip blank lines before the card starts
  go Initial          q a BlankLine            = (Initial, q, a)
  -- Start of the front of the card
  go Initial          q a (StartSection Front) = (Await Front, q, a)

  -- Skip blank lines before the front starts
  go (Await Front   ) q a BlankLine            = (Await Front, q, a)
  -- First nonempty line of the front
  go (Await Front   ) _ a (LineOfText t)       = (Inside Front 0, t, a)

  -- Add a newline to the pending queue
  go (Inside Front n) q a BlankLine            = (Inside Front (n + 1), q, a)
  -- Append text and any pending newlines
  go (Inside Front n) q a (LineOfText t) = (Inside Front 0, q <> ns <> t, a)
    where ns = BS.replicate (n + 1) newlineWord8
  -- On reaching the back marker, discard any newlines
  go (Inside Front _) q a (StartSection Back) = (Await Back, q, a)

  -- Skip preceding newlines
  go (Await Back    ) q a BlankLine           = (Await Back, q, a)
  -- First nonempty line
  go (Await Back    ) q _ (LineOfText t)      = (Inside Back 0, q, t)

  -- Add a newline to the queue
  go (Inside Back n ) q a BlankLine           = (Inside Back (n + 1), q, a)
  -- Append text and any pending newlines
  go (Inside Back n ) q a (LineOfText t)      = (Inside Back 0, q, a <> ns <> t)
    where ns = BS.replicate (n + 1) newlineWord8

  go s q a t = error (unlines [show s, show q, show a, show t])

newlineWord8 :: Word8
newlineWord8 = fromIntegral (fromEnum '\n')

makeCard :: Day -> (BS.ByteString, BS.ByteString) -> Card Insert
makeCard curr (front, back) = Card
  { _cardId   = InsertDefault
  , _easeFcr  = lit defaultEaseFcr
  , _repsDone = lit 0
  , _nextDue  = lit curr
  , _interval = lit (DayInterval 0)
  , _front    = lit (TE.decodeUtf8 front)
  , _back     = lit (TE.decodeUtf8 back)
  }

exprOf :: CardQ -> CardE
exprOf Card {..} = Card
  { _interval = lit _interval
  , _cardId   = lit _cardId
  , _easeFcr  = lit _easeFcr
  , _repsDone = lit _repsDone
  , _nextDue  = lit _nextDue
  , _front    = lit _front
  , _back     = lit _back
  }

clearAllCards :: IO Int64
clearAllCards =
  PGS.connect connInfo >>= \conn -> R.delete @Card conn (\_ -> lit True)

loadCards :: FilePath -> IO ()
loadCards file = do
  conn <- PGS.connect connInfo
  curr <- utctDay <$> getCurrentTime
  -- R.delete @Card conn (\_ -> lit True)
  withFile file ReadMode $ streamCardsFrom >>> S.mapM_
    ( \(front, back) -> do
      let front' = TE.decodeUtf8 front
          back'  = TE.decodeUtf8 back
      l <- runResourceT
        (S.length_ (R.select (R.stream conn) (cardsWithFront front')))
      if l == 0
        then void $ do
          T.putStr "New card: "
          R.insert @Card conn [makeCard curr (front, back)]
        else T.putStrLn "Skipping card: "
      T.putStrLn (front' <> " / " <> back' <> "\n")
    )

cardsWithFront :: Text -> O.Query (Card Expr)
cardsWithFront t = filterQuery (\c -> c ^. front ==. lit t) allCards

----------------------------------------------------------------------

formattedTime :: Day -> Text
formattedTime = T.pack . formatTime defaultTimeLocale "%B %d, %Y"

cardsDueOn :: Day -> O.Query (Card Expr)
cardsDueOn today = filterQuery (\c -> c ^. nextDue ==. lit today) allCards

demo :: IO ()
demo = do
  conn <- PGS.connect connInfo
  curr <- utctDay <$> getCurrentTime
  T.putStrLn ("Drill for " <> formattedTime curr)
  runResourceT $ flip S.mapM_ (R.select (R.stream conn) (cardsDueOn curr)) $ \c -> do
    lift $ do
      T.putStrLn ("\n#" <> tshow (c ^. cardId . _CardId))
      T.putStrLn ("\nFront:\n" <> c ^. front)
      T.putStrLn ("\nBack:\n" <> c ^. back)
      T.putStrLn "\nMetadata:"
      T.putStrLn ("  " <> tshow (c ^. easeFcr))
      T.putStrLn ("  " <> tshow (c ^. interval))
      T.putStrLn ("  " <> c ^. nextDue . to formattedTime)
      T.putStrLn ""
      putStr "Enter quality (0-5 inclusive): "

    i' <- AT.parseOnly parseQuality <$> lift T.getLine
    case i' of
      Left  _ -> lift (T.putStrLn "Error: bad quality entered, skipping.")
      Right i -> do
        let upd = updateCard curr c i

        R.update conn
                 (\c' -> lit (c ^. cardId) ==. c' ^. cardId)
                 (\_ -> exprOf upd)

        lift $ do
          T.putStrLn "\nUpdated card:"
          T.putStrLn ("  " <> tshow (upd ^. easeFcr))
          T.putStrLn ("  " <> tshow (upd ^. interval))
          T.putStrLn ("  " <> upd ^. nextDue . to formattedTime)
          T.putStrLn "\n------"
  pending <- runResourceT $ S.length_ (R.select (R.stream conn) (cardsDueOn curr))
  T.putStrLn ("Finished drill for " <> formattedTime curr <> ". " <> tshow pending <> " card(s) pending.")
  when (pending > 0) $ do
    T.putStr "\nReview pending cards? (y/n): "
    yn <- T.getLine
    when (yn == "y") demo

allCards :: O.Query (Card Expr)
allCards = queryTable

tshow :: Show a => a -> Text
tshow = T.pack . show

main :: IO ()
main = demo
