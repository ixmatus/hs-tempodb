{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Tempodb.Types where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as C8
import           Data.Int
import           Data.Map              (Map)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Time
import           Data.Typeable         (Typeable)
import           Network.Http.Client
import           Prelude               as P
import           System.Locale

-- | It's easy to mix up which one is first so let's newtype these
-- suckers to make it explicit.
newtype ApiKey = ApiKey {unKey :: ByteString} deriving (Show, Eq, Ord)
newtype ApiSec = ApiSec {unSec :: ByteString} deriving (Show, Eq, Ord)

newtype TempoDBTime = TempoDBTime {
      fromTempoDBTime :: UTCTime
    } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

data BasicAuth = BasicAuth ApiKey ApiSec
    deriving (Show, Eq, Ord)

-- | Custom TempoDB ReaderT monad.
newtype Tempodb a = Tempodb {
    runTDB :: ReaderT (RequestBuilder ()) IO a
    } deriving (Monad, MonadIO, MonadReader (RequestBuilder ()))

runTempoDB :: BasicAuth -> Tempodb a -> IO a
runTempoDB auth f = runReaderT (runTDB f) $ baseRequest auth

baseRequest :: BasicAuth -> RequestBuilder ()
baseRequest (BasicAuth k s) =
    setAuthorizationBasic (unKey k) (unSec s)

data IdOrKey = SeriesId ByteString | SeriesKey ByteString
    deriving (Show, Eq, Ord)

-- | Datatype for TempoDB Series Metadata.
data Series = Series
    { id         :: ByteString
    , key        :: ByteString
    , name       :: ByteString
    , tags       :: [ByteString]
    , attributes :: Map T.Text T.Text
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

data Num a => Data a = Data
    { uid       :: Maybe IdOrKey
    , timestamp :: Maybe TempoDBTime
    , value     :: a
    } deriving (Show, Eq, Ord)

data Bulk a = Bulk
    { timestmp   :: TempoDBTime
    , bulkValues :: [Data a]
    } deriving (Show, Eq, Ord)

data Rollup = Rollup
    { interval :: ByteString
    , function :: ByteString
    , tz       :: ByteString
    } deriving (Show, Eq, Ord)

data Summary = Summary
    { mean   :: Double
    , sum    :: Double
    , min    :: Double
    , max    :: Double
    , stddev :: Double
    , ss     :: Double
    , count  :: Int
    } deriving (Show, Eq, Ord)

data SeriesData a = SeriesData
    { series  :: Series
    , start   :: TempoDBTime
    , end     :: TempoDBTime
    , values  :: [Data a]
    , rollup  :: Maybe Rollup
    , summary :: Summary
    } deriving (Show, Eq, Ord)

instance FromJSON (SeriesData Int64) where
    parseJSON (Object o) = SeriesData    <$>
                           o .: "series" <*>
                           o .: "start"  <*>
                           o .: "end"    <*>
                           o .: "data"   <*>
                           o .: "rollup" <*>
                           o .: "summary"

    parseJSON _ = mzero

instance FromJSON (Bulk Int64) where
    parseJSON (Object o) = Bulk     <$>
                           o .: "t" <*>
                           o .: "data"
    parseJSON _ = mzero

instance ToJSON (Bulk Int64) where
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

instance FromJSON (Data Int64) where
    parseJSON = parseSeriesData

parseSeriesData :: Value -> Parser (Data Int64)
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

instance ToJSON (Data Int64) where
    toJSON = buildSeriesData

buildSeriesData :: (Data Int64) -> Value
buildSeriesData (Data i t v) = object . ts . eid $ ["v" .= v]
  where
    ts l = case t of
        Nothing -> l

        Just tv -> ("t", toJSON tv):l
    eid l = case i of
        Nothing -> l
        Just (SeriesId idv) -> ("id" .= idv):l
        Just (SeriesKey kv) -> ("key".= kv) :l
