{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Methods where

import           Control.Monad.Reader
import           Data.ByteString.Char8  as C8
import           Database.Tempodb.Types
import           Network.HTTP.Base      (urlEncodeVars)
import           Network.Http.Client
import           OpenSSL                (withOpenSSL)

-- | Top-level API methods are:
--
-- 1. Series
-- 2. Read
-- 3. Write
-- 4. Increment
-- 5. Single
-- 6. Delete

type QueryArgs = [(String, String)]

-- | Root API version path.
rootpath :: ByteString
rootpath = "/v1"

runRequest :: Request -> IO ByteString
runRequest r = withOpenSSL $ do
    withConnection (establishConnection "https://api.tempo-db.com") go
  where
    go c = do
        sendRequest c r emptyBody
        receiveResponse c concatHandler'

--seriesGet
seriesList :: Maybe QueryArgs -> Tempodb ByteString
seriesList q = do
    let root = C8.concat [rootpath, "/", "series"]
        path = case q of
          Nothing  -> root
          Just qry -> C8.concat [root, "?", C8.pack $ urlEncodeVars qry]

    auth <- ask
    req  <- liftIO . buildRequest $ do
        http GET path
        auth

    liftIO $ runRequest req


--seriesCreate
--seriesUpdate
--seriesDelete
