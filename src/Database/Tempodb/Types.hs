{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Tempodb.Types where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString.Char8 as C8
import           Data.Map              (Map)
import qualified Data.Text             as T
import           Data.Time
import           Network.Http.Client
import           Prelude               as P
import           System.Locale

-- | It's easy to mix up which one is first so let's newtype these
-- suckers to make it explicit.
newtype ApiKey = ApiKey {unKey :: ByteString} deriving (Show, Eq, Ord)
newtype ApiSec = ApiSec {unSec :: ByteString} deriving (Show, Eq, Ord)

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
    , attributes :: Map ByteString ByteString
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

data Data = Data
    { timestamp :: UTCTime
    , value     :: Double
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

data SeriesData = SeriesData
    { series  :: Series
    , start   :: UTCTime
    , end     :: UTCTime
    , values  :: [Data]
    , rollup  :: Maybe Rollup
    , summary :: Summary
    } deriving (Show, Eq, Ord)

instance FromJSON SeriesData where
    parseJSON (Object o) = SeriesData    <$>
                           o .: "series" <*>
                           o .: "start"  <*>
                           o .: "end"    <*>
                           o .: "data"   <*>
                           o .: "rollup" <*>
                           o .: "summary"

    parseJSON _ = mzero

instance FromJSON Data where
    parseJSON (Object o) = Data     <$>
                           o .: "t" <*>
                           o .: "v"
    parseJSON _ = mzero

instance ToJSON Data where
    toJSON = buildData

buildData :: Data -> Value
buildData (Data t v) = object
    [ timest
    , "v" .= v
    ]
  where
    timest = ("t", String . T.pack $ formatTime defaultTimeLocale "%FT%H:%M:%S%Q%z" t)

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
