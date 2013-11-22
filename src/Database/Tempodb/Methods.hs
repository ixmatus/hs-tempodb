{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Methods where

import           Control.Monad.Reader
import           Data.Aeson             as A
import           Data.ByteString.Char8  as C8
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Monoid
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


seriesGet :: IdOrKey -> Tempodb (Maybe Series)
seriesGet q = do
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http GET path
        auth

    result <- liftIO $ runRequest req
    return . A.decode $ fromStrict result

  where
    ident (SeriesId i)   = "/id/" <> i
    ident (SeriesKey k) = "/key" <> k
    path = rootpath <> "/" <> "series/" <> (ident q)

seriesList :: Maybe QueryArgs -> Tempodb ByteString
seriesList q = do
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http GET path
        auth

    liftIO $ runRequest req

  where
    root = rootpath <> "/" <> "series"
    path = case q of
        Nothing  -> root
        Just qry -> root <> "?" <> (C8.pack $ urlEncodeVars qry)


--seriesCreate
--seriesUpdate
--seriesDelete
