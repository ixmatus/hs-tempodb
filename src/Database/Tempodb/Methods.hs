{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Methods where

import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Monad.Reader
import           Data.Aeson                          as A
import           Data.ByteString.Char8               as C8
import           Data.ByteString.Lazy                (fromStrict, toStrict)
import           Data.Monoid
import           Database.Tempodb.Types
import           Network.HTTP.Base                   (urlEncodeVars)
import           Network.Http.Client
import           OpenSSL                             (withOpenSSL)
import           System.IO.Streams                   (write)

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

seriesCreate :: ByteString -> Tempodb (Maybe Series)
seriesCreate k = do
    let postdata = "{\"key\": \"" <> k <> "\"}"
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http POST $ rootpath <> "/series"
        setContentLength . fromIntegral $ C8.length postdata
        auth

    result <- liftIO . runRequest req $ Just postdata
    return . A.decode $ fromStrict result

seriesGet :: IdOrKey -> Tempodb (Maybe Series)
seriesGet q = do
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http GET path
        auth

    result <- liftIO $ runRequest req Nothing
    return . A.decode $ fromStrict result

  where
    ident (SeriesId i)   = "/id/" <> i
    ident (SeriesKey k) = "/key/" <> k
    path = rootpath <> "/series" <> (ident q)

seriesList :: Maybe QueryArgs -> Tempodb (Maybe [Series])
seriesList q = do
    req <- seriesCommon q GET
    result <- liftIO $ runRequest req Nothing
    return . A.decode $ fromStrict result

seriesDelete :: Maybe QueryArgs -> Tempodb ByteString
seriesDelete q = do
    req <- seriesCommon q DELETE
    liftIO $ runRequest req Nothing

seriesCommon :: Maybe QueryArgs -> Method -> Tempodb Request
seriesCommon q method = do
    auth <- ask
    liftIO . buildRequest $ do
        http method path
        setContentLength 0
        auth

  where
    root = rootpath <> "/series"
    path = case q of
        Nothing  -> root
        Just qry -> root <> "?" <> (C8.pack $ urlEncodeVars qry)

seriesUpdate :: IdOrKey -> Series -> Tempodb (Maybe Series)
seriesUpdate q s = do
    let postdata = toStrict $ A.encode s
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http PUT path
        setContentLength . fromIntegral $ C8.length postdata
        auth

    result <- liftIO . runRequest req $ Just postdata
    return . A.decode $ fromStrict result

  where
    ident (SeriesId i)   = "/id/" <> i
    ident (SeriesKey k) = "/key/" <> k
    path = rootpath <> "/series" <> (ident q)
