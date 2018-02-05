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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Applicative
import           Control.Arrow
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
import qualified Streaming.Prelude                    as S

import Prelude hiding (Int, Integer)

----------------------------------------------------------------------

newtype EaseFcr = EaseFcr Double
  deriving (Show, Ord, Eq, DBType, FromField)

unEaseFcr :: EaseFcr -> Double
unEaseFcr (EaseFcr ef) = ef

newtype Quality = Quality Int16
  deriving (Show, DBType, FromField)

unQuality :: Quality -> Int16
unQuality (Quality q) = q

newtype Interval = Interval Int64
  deriving (Show, DBType, FromField)

unInterval :: Interval -> Int64
unInterval (Interval i) = i

----------------------------------------------------------------------

mkQuality :: Int16 -> Quality
mkQuality !n | n < 0 || n > 5 = error "quality out of bounds"
             | otherwise      = Quality n

mkEaseFcr :: Double -> EaseFcr
mkEaseFcr !n | n < 1.2 || n > 3.0 = error "easefcr out of bounds"
             | otherwise          = EaseFcr n

defaultEaseFcr :: EaseFcr
defaultEaseFcr = EaseFcr 2.5

defaultInterval :: Int64
defaultInterval = 1

lowerThresholdEaseFcr :: EaseFcr
lowerThresholdEaseFcr = EaseFcr 1.3

addIntervalToUTC :: Interval -> UTCTime -> UTCTime
addIntervalToUTC (Interval s) = addUTCTime (fromIntegral s)

----------------------------------------------------------------------

nextRepIval :: EaseFcr -> Quality -> Interval
nextRepIval (EaseFcr ef) (Quality q) = Interval (86400 * round (go ef q))
 where
  go _  1 = fromIntegral defaultInterval
  go _  2 = 6
  go ef n = 6 * ef ** (fromIntegral n - 2)
  -- go item n = go item (n - 1) * (item ^. easeFcr . to unEaseFcr)

modifiedEaseFcr :: EaseFcr -> Quality -> EaseFcr
modifiedEaseFcr (EaseFcr ef) (Quality (fromIntegral -> q)) = max
  lowerThresholdEaseFcr
  updatedEaseFcr
 where
  delta          = 0.1 - 0.04 * (5 - q) * (9 - q)
  updatedEaseFcr = EaseFcr (ef + delta)

----------------------------------------------------------------------

newtype CardId = CardId Int64
  deriving (Show, DBType, FromField)

makePrisms ''CardId

data Card f = Card
  { _cardId   :: C f "id"          'HasDefault CardId
  , _easeFcr  :: C f "ease_factor" 'NoDefault  EaseFcr
  , _repsDone :: C f "reps_done"   'NoDefault  Int64
  , _nextDue  :: C f "next_due"    'NoDefault  UTCTime
  , _interval :: C f "interval"    'NoDefault  Interval
  , _front    :: C f "front"       'NoDefault  Text
  , _back     :: C f "back"        'NoDefault  Text
  } deriving (Generic)

makeLenses ''Card

instance BaseTable Card where tableName = "card"

instance (e ~ Expr, q ~ QueryResult) => Table (Card e) (Card q)

deriving instance (q ~ QueryResult) => Show (Card q)

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

demo :: IO ()
demo = do
  conn <- PGS.connect connInfo
  -- R.update conn
  --          (\part -> _partId part ==. lit 5)
  --          (\part -> part { _partColor = lit "red" })
  -- R.delete @Part @Bool conn (\_ -> lit True)
  curr <- getCurrentTime
  runResourceT $ S.mapM_ (lift . print) $ 
    R.insertReturning
      (R.stream conn)
      [ Card
          { _cardId   = InsertDefault
          , _easeFcr  = lit defaultEaseFcr
          , _repsDone = lit 0
          , _nextDue  = lit curr
          , _interval = lit (Interval 0)
          , _front    = lit "Question?"
          , _back     = lit "Answer"
          }
      ]

  putStrLn "Cards now:"
  runResourceT $ flip S.mapM_ (R.select (R.stream conn) allCards) $ \c -> 
    lift (T.putStrLn (tshow (c ^. cardId . _CardId) <> ": " <> c ^. front <> " / " <> c ^. back))

allCards :: O.Query (Card Expr)
allCards = queryTable

tshow :: Show a => a -> Text
tshow = T.pack . show

main :: IO ()
main = demo
