{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Tempodb.Types
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
----------------------------------------------------------------------------

module Database.Tempodb.Types
(
-- * API Authentication Types
  ApiKey(unKey)
, ApiSec(unSec)
, BasicAuth
-- * TempoDB Monad
, Tempodb(runTDB)
, runTempoDB
, TempoDBTime(fromTempoDBTime)
-- * General TempoDB Types
-- ** Series ID or Key
, IdOrKey(SeriesId, SeriesKey)
-- ** Series Objects
, Series(id, key, name, tags, attributes)
-- ** Data Units
, Data(uid, timestamp, value)
, compare
-- ** Read-specific Series Data Types
, Rollup(interval, function, tz)
, Summary(mean, sum, min, max, stddev, ss, count)
, SeriesData(series, start, end, values, rollup, summary)
, Bulk(timestmp, bulkValues)
)
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as C8
import           Data.Map              (Map)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Time
import           Data.Typeable         (Typeable)
import           Network.Http.Client
import           Prelude               as P
import           System.Locale

-- | Your TempoDB API Key representing the first part of a `BasicAuth`
-- value.
newtype ApiKey = ApiKey {unKey :: ByteString} deriving (Show, Eq, Ord)

-- | Your TempoDB API Secret representing the second part of a `BasicAuth`
-- value.
newtype ApiSec = ApiSec {unSec :: ByteString} deriving (Show, Eq, Ord)

-- | Representing a BasicAuth value - the fields are explicit because
-- it's easy to forget which ORDER each is, particularly when they're
-- just hashes!
data BasicAuth = BasicAuth ApiKey ApiSec
    deriving (Show, Eq, Ord)

-- | Custom TempoDB ReaderT monad.
newtype Tempodb a = Tempodb {
    runTDB :: ReaderT (RequestBuilder ()) IO a
    } deriving (Monad, MonadIO, MonadReader (RequestBuilder ()))

-- | Run an action (`Database.Tempodb.Methods`) inside the `Tempodb`
-- monad with a BasicAuth value.
runTempoDB :: BasicAuth -> Tempodb a -> IO a
runTempoDB auth f = runReaderT (runTDB f) $ baseRequest auth

-- | Set basic authorization using the `BasicAuth` value.
baseRequest :: BasicAuth -> RequestBuilder ()
baseRequest (BasicAuth k s) =
    setAuthorizationBasic (unKey k) (unSec s)

newtype TempoDBTime = TempoDBTime {
      fromTempoDBTime :: UTCTime
    } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

-- | IdOrKey is a bad data type name.
data IdOrKey = SeriesId ByteString | SeriesKey ByteString
    deriving (Show, Eq, Ord)

-- | TempoDB series metadata.
data Series = Series
    { id         :: !ByteString
    , key        :: !ByteString
    , name       :: !ByteString
    , tags       :: ![ByteString]
    , attributes :: Map T.Text T.Text
    } deriving (Show, Eq, Ord)

-- | A TempoDB unit of data, it may have a unique series Id or
-- Timestamp.
data Data = Data
    { uid       :: Maybe IdOrKey
    , timestamp :: Maybe TempoDBTime
    , value     :: Double
    } deriving (Show, Eq)

-- | So we can sort by timestamp.
instance Ord Data where
    compare (Data _ (Just t) _) (Data _ (Just t') _) = compare t t'
    -- compare (Data _ Nothing _) (Data _ t' _)         = compare Nothing t'
    -- compare (Data _ Nothing v) (Data _ Nothing v')   = compare v v'

-- | A TempoDB bulk write will write a list of `Data` units (with
-- either the same Id or different ones!) with a single timestamp.
data Bulk = Bulk
    { timestmp   :: TempoDBTime
    , bulkValues :: [Data]
    } deriving (Show, Eq, Ord)

-- | Represent a rollup (downsampling) specification. Not entirely
-- sure if I should be using record syntax here.
data Rollup = Rollup
    { interval :: ByteString
    , function :: ByteString
    , tz       :: ByteString
    } deriving (Show, Eq, Ord)

-- | Represent a series' summary. Also not sure if I need record
-- syntax here.
data Summary = Summary
    { mean   :: Double
    , sum    :: Double
    , min    :: Double
    , max    :: Double
    , stddev :: Double
    , ss     :: Double
    , count  :: Int
    } deriving (Show, Eq, Ord)

-- | Represent a series' data including it's range, rollup
-- specification, and summary.
data SeriesData = SeriesData
    { series  :: Series
    , start   :: TempoDBTime
    , end     :: TempoDBTime
    , values  :: [Data]
    , rollup  :: Maybe Rollup
    , summary :: Summary
    } deriving (Show, Eq, Ord)

instance FromJSON Series where
    parseJSON (Object o) = Series      <$>
                           o .: "id"   <*>
                           o .: "key"  <*>
                           o .: "name" <*>
                           o .: "tags" <*>
                           o .: "attributes"
    parseJSON _ = mzero

instance ToJSON Series where
    toJSON (Series i k n t a) = object
        [ "id"    .= i
        , "key"   .= k
        , "name"  .= n
        , "tags"  .= t
        , "attributes" .= a
        ]

instance ToJSON B.ByteString where
    toJSON = String . decodeUtf8
    {-# INLINE toJSON #-}

instance FromJSON B.ByteString where
    parseJSON = withText "ByteString" $ pure . encodeUtf8
    {-# INLINE parseJSON #-}


instance FromJSON SeriesData where
    parseJSON (Object o) = SeriesData    <$>
                           o .: "series" <*>
                           o .: "start"  <*>
                           o .: "end"    <*>
                           o .: "data"   <*>
                           o .: "rollup" <*>
                           o .: "summary"

    parseJSON _ = mzero

instance FromJSON Bulk where
    parseJSON (Object o) = Bulk     <$>
                           o .: "t" <*>
                           o .: "data"
    parseJSON _ = mzero

instance ToJSON Bulk where
    toJSON (Bulk t v) = object
        [ "t"    .= t
        , "data" .= v
        ]

instance FromJSON Rollup where
    parseJSON (Object o) = Rollup          <$>
                           o .: "interval" <*>
                           o .: "function" <*>
                           o .: "tz"
    parseJSON _ = mzero

instance FromJSON Summary where
    parseJSON (Object o) = Summary       <$>
                           o .: "mean"   <*>
                           o .: "sum"    <*>
                           o .: "min"    <*>
                           o .: "max"    <*>
                           o .: "stddev" <*>
                           o .: "ss"     <*>
                           o .: "count"
    parseJSON _ = mzero

instance ToJSON TempoDBTime where
    toJSON (TempoDBTime t) =
        String (T.pack (formatTime defaultTimeLocale "%FT%H:%M:%S%Q%z" t))

instance FromJSON TempoDBTime where
    parseJSON (String t) =
        case parseTime defaultTimeLocale "%FT%H:%M:%S%Q%z" (T.unpack t) of
          Just d  -> pure (TempoDBTime d)
          Nothing ->
              case parseTime defaultTimeLocale "%FT%H:%M:%S%Q%Z" (T.unpack t) of
                  Just d  -> pure (TempoDBTime d)
                  Nothing -> mzero
    parseJSON _          = mzero

instance FromJSON Data where
    parseJSON = parseSeriesData

parseSeriesData :: Value -> Parser Data
parseSeriesData v = do
    case v of
        Object o -> do
            -- Is the ID there? Or is the key element there?
            kid <- (o .:? "id") >>= isUID o
            ts  <- o .:? "t"
            val <- o .: "v"
            return $ Data kid ts val
        _ -> mzero
  where
    isUID o i = case i of
        Nothing    -> (o .:? "key") >>= isKEY
        Just idval -> return . Just $ SeriesId idval
    isKEY k = case k of
        Nothing    -> return Nothing
        Just idkey -> return . Just $ SeriesKey idkey

instance ToJSON Data where
    toJSON = buildSeriesData

buildSeriesData :: Data -> Value
buildSeriesData (Data i t v) = object . ts . eid $ ["v" .= v]
  where
    ts l = case t of
        Nothing -> l

        Just tv -> ("t", toJSON tv):l
    eid l = case i of
        Nothing -> l
        Just (SeriesId idv) -> ("id" .= idv):l
        Just (SeriesKey kv) -> ("key".= kv) :l
