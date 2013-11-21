{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Tempodb.Types where

import           Control.Monad.Reader

import           Data.ByteString.Base64
import           Data.ByteString.Char8  as C8

-- | It's easy to mix up which one is first so let's newtype these
-- suckers to make it explicit.
newtype ApiKey = ApiKey ByteString deriving (Show, Eq, Ord)
newtype ApiSec = ApiSec ByteString deriving (Show, Eq, Ord)

data BasicAuth = BasicAuth ApiKey ApiSec
    deriving (Show, Eq, Ord)

-- | Unwrap the key and secret, concatenate with a colon, and base64
-- encode the result.
authEncode :: BasicAuth -> ByteString
authEncode (BasicAuth k s) =
    let (ApiKey kb) = k
        (ApiSec ks) = s
    in encode $ C8.concat [kb, ":", ks]

-- | Custom TempoDB ReaderT.
newtype Tempodb a = Tempodb {
    runTDB :: ReaderT BasicAuth IO a
    } deriving (Monad, MonadIO, MonadReader BasicAuth)

runTempoDB :: Tempodb a -> BasicAuth -> IO (a)
runTempoDB k auth = runReaderT (runTDB k) auth
