{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Tempodb.Types where

import           Control.Monad.Reader
import           Data.ByteString.Char8 as C8
import           Network.Http.Client

-- | It's easy to mix up which one is first so let's newtype these
-- suckers to make it explicit.
newtype ApiKey = ApiKey {unKey :: ByteString} deriving (Show, Eq, Ord)
newtype ApiSec = ApiSec {unSec :: ByteString} deriving (Show, Eq, Ord)

data BasicAuth = BasicAuth ApiKey ApiSec
    deriving (Show, Eq, Ord)

-- | Custom TempoDB ReaderT.
newtype Tempodb a = Tempodb {
    runTDB :: ReaderT (RequestBuilder ()) IO a
    } deriving (Monad, MonadIO, MonadReader (RequestBuilder ()))

runTempoDB :: BasicAuth -> Tempodb a -> IO a
runTempoDB auth f = runReaderT (runTDB f) $ baseRequest auth

baseRequest :: BasicAuth -> RequestBuilder ()
baseRequest (BasicAuth k s) =
    setAuthorizationBasic (unKey k) (unSec s)
