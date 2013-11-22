{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Tempodb.Types where

import           Prelude               as P

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString.Char8 as C8
import           Data.Map              (Map)
import           Network.Http.Client

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

newtype AttrList = AttrList { attrList :: [(ByteString, ByteString)]}
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
