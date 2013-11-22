{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Util where

import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Data.ByteString.Char8               as C8
import           Network.Http.Client
import           OpenSSL                             (withOpenSSL)
import           System.IO.Streams                   (write)

type QueryArgs = [(String, String)]

-- | Root API version path.
rootpath :: ByteString
rootpath = "/v1"

-- | Run a constructed request.
runRequest :: Request -> Maybe ByteString -> IO ByteString
runRequest r b = withOpenSSL $ do
    withConnection (establishConnection "https://api.tempo-db.com") go
  where
    body = case b of
      Nothing -> emptyBody
      Just v  -> (\o -> write (Just $ fromByteString v) o)
    go c = do
        sendRequest c r body
        receiveResponse c concatHandler'
