{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
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
import qualified Rel8.IO                              as R
import           Streaming
import qualified Streaming.Prelude                    as S

import           Prelude                              hiding (Int, Integer)

import qualified Data.Attoparsec.ByteString           as ABS
import qualified Data.Attoparsec.Text                 as AT

import qualified Data.Attoparsec.ByteString           as A
import qualified Data.Attoparsec.ByteString.Char8     as AQ
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Streaming.Char8      as Q
import qualified Data.Text.Encoding                   as TE

import           System.IO                            (IOMode (..), withFile)

import           Control.Arrow                        ((>>>))

getCardData :: FilePath -> IO [(Text, Text)]
getCardData file = withFile
  file
  ReadMode
  (   Q.fromHandle
  >>> A.parsed parseTwoSided
  >>> void
  >>> S.map (\(a, b) -> (f a, f b))
  >>> S.toList_
  )
  where f = TE.decodeUtf8

parseTwoSided :: A.Parser (BS.ByteString, BS.ByteString)
parseTwoSided = (,) <$> front <*> back
 where
  front = BS.pack <$> A.manyTill A.anyWord8 (A.string "\n<<<\n")
  back  = BS.pack <$> A.manyTill A.anyWord8 (A.string "\n>>>\n")

----------------------------------------------------------------------

newtype EaseFcr = EaseFcr Double
  deriving (Show, Ord, Eq, DBType, FromField)

unEaseFcr :: EaseFcr -> Double
unEaseFcr (EaseFcr ef) = ef

newtype Quality = Quality Int16
  deriving (Show, DBType, FromField)

unQuality :: Quality -> Int16
unQuality (Quality q) = q

newtype Days = Days Int64
  deriving (Show, DBType, FromField)

unDays :: Days -> Int64
unDays (Days i) = i

----------------------------------------------------------------------

mkQuality :: Int16 -> Quality
mkQuality !n | n < 0 || n > 5 = error "quality out of bounds"
             | otherwise      = Quality n

mkEaseFcr :: Double -> EaseFcr
mkEaseFcr !n | n < 1.2 || n > 3.0 = error "easefcr out of bounds"
             | otherwise          = EaseFcr n

defaultEaseFcr :: EaseFcr
defaultEaseFcr = EaseFcr 2.5

defaultDaysBase :: Int64
defaultDaysBase = 1

defaultDays :: Int64
defaultDays = 6

lowerThresholdEaseFcr :: EaseFcr
lowerThresholdEaseFcr = EaseFcr 1.3

addDaysToUTC :: Days -> UTCTime -> UTCTime
addDaysToUTC (Days s) = addUTCTime (86400 * fromIntegral s)

----------------------------------------------------------------------

-- | Calculate the interval till the next repetition based
-- on the quality of a recall event.
nextRepIval :: EaseFcr -> Quality -> Days
nextRepIval (EaseFcr ef) (Quality q) = Days (round (go ef q))
 where
  go _  1 = fromIntegral defaultDaysBase
  go _  2 = fromIntegral defaultDays
  go ef n = fromIntegral defaultDays * ef ** (fromIntegral n - 2)
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
  , _nextDue  :: C f "next_due"    'NoDefault  UTCTime
  , _interval :: C f "interval"    'NoDefault  Days
  , _front    :: C f "front"       'NoDefault  Text
  , _back     :: C f "back"        'NoDefault  Text
  } deriving (Generic)

makeLenses ''Card

type CardQ = Card QueryResult
type CardE = Card Expr

instance BaseTable Card where tableName = "card"

instance (e ~ Expr, q ~ QueryResult) => Table (Card e) (Card q)

deriving instance (q ~ QueryResult) => Show (Card q)

updateCard :: UTCTime -> CardQ -> Quality -> CardQ
updateCard curr c@Card {..} q = c { _repsDone = _repsDone'
                                  , _nextDue  = _nextDue'
                                  , _interval = _interval'
                                  , _easeFcr  = _easeFcr'
                                  }
 where
  bad        = unQuality q < 3

  _interval' = nextRepIval _easeFcr q
  _nextDue'  = addDaysToUTC _interval' curr

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

parseQuality :: AT.Parser Quality
parseQuality = Quality <$> AT.decimal

-- test :: Text
-- test = T.unlines [ "Integral substitution for $(ax^2 + bx + c)^{1/2}$ with $a > 0$."
--                  , "~~~~~"
--                  , "$(ax^2 + bx + c)^{1/2} = t - \\sqrt{a}x"
--                  , "!!"
--                  , "~~~~~"
--                  ]

loadCards :: IO ()
loadCards = do
  conn  <- PGS.connect connInfo
  curr  <- getCurrentTime
  cards <- getCardData "cards.txt"
  for_ cards $ \(front, back) ->
    runResourceT $ S.mapM_ (lift . print) $ R.insertReturning
      (R.stream conn)
      [ Card
          { _cardId   = InsertDefault
          , _easeFcr  = lit defaultEaseFcr
          , _repsDone = lit 0
          , _nextDue  = lit curr
          , _interval = lit (Days 0)
          , _front    = lit front
          , _back     = lit back
          }
      ]

demo :: IO ()
demo = do
  conn <- PGS.connect connInfo
  curr <- getCurrentTime
  -- R.delete @Card @Bool conn (\_ -> lit True)
  runResourceT $ flip S.mapM_ (R.select (R.stream conn) allCards) $ \c -> do
    lift $ do
      T.putStrLn ("\n#" <> tshow (c ^. cardId . _CardId))
      T.putStrLn ("\nFront:\n" <> c ^. front)
      T.putStrLn ("\nBack:\n" <> c ^. back)
      T.putStrLn "\n"
      T.putStrLn (tshow (c ^. easeFcr))
      T.putStrLn (tshow (c ^. interval))
      T.putStrLn ""
      putStr "Quality: "

    i' <- AT.parseOnly parseQuality <$> lift T.getLine
    case i' of
      Left  _ -> lift (T.putStrLn "Error: bad quality")
      Right i -> do
        let upd = updateCard curr c i

        R.update conn
                 (\c' -> lit (c ^. cardId) ==. c' ^. cardId)
                 (\_ -> exprOf upd)

        lift $ do
          T.putStrLn ""
          T.putStrLn (tshow (upd ^. easeFcr))
          T.putStrLn (tshow (upd ^. interval))
          T.putStrLn "\n------"

allCards :: O.Query (Card Expr)
allCards = queryTable

tshow :: Show a => a -> Text
tshow = T.pack . show

main :: IO ()
main = demo
