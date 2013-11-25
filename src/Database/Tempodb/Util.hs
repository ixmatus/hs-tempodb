{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module Database.Tempodb.Util where

import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Exception                   (Exception, throw)
import           Data.ByteString.Char8               as C8
import           Data.Typeable
import           Network.Http.Client
import           OpenSSL                             (withOpenSSL)
import           System.IO.Streams                   (InputStream, write)

type QueryArgs = [(String, String)]

-- | Root API version path.
rootpath :: ByteString
rootpath = "/v1"

-- | Run a constructed request.
runRequest :: Request -> Maybe ByteString -> IO (Int,ByteString)
runRequest r b = withOpenSSL $ do
    withConnection (establishConnection "https://api.tempo-db.com") go
  where
    body = case b of
      Nothing -> emptyBody
      Just v  -> (\o -> write (Just $ fromByteString v) o)
    go c = do
        sendRequest c r body
        receiveResponse c concatHandlerSt'

concatHandlerSt' :: Response -> InputStream ByteString -> IO (StatusCode,ByteString)
concatHandlerSt' p i =
    if s >= 300
        then throw (HttpClientError s m)
        else (concatHandler p i) >>= return . (s,)
  where
    s = getStatusCode p
    m = getStatusMessage p

data HttpClientError = HttpClientError Int ByteString
        deriving (Typeable)

instance Exception HttpClientError

instance Show HttpClientError where
    show (HttpClientError s msg) = Prelude.show s ++ " " ++ C8.unpack msg
