{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Methods.Read
( readOne
, readMulti
)
where

import           Control.Monad.Reader
import           Data.Aeson             as A
import           Data.ByteString.Char8  as C8
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Monoid
import           Database.Tempodb.Types
import           Database.Tempodb.Util
import           Network.HTTP.Base      (urlEncodeVars)
import           Network.Http.Client

readOne :: IdOrKey -> Maybe QueryArgs -> Tempodb (Maybe SeriesData)
readOne q qa = do
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http GET path
        auth

    (_,result) <- liftIO $ runRequest req Nothing
    return . A.decode $ fromStrict result

  where
    ident (SeriesId i)   = "/id/" <> i
    ident (SeriesKey k) = "/key/" <> k

    path = rootpath <> "/series" <> (ident q) <> "/data" <> query

    query = case qa of
        Nothing  -> mempty
        Just qry -> "?" <> (C8.pack $ urlEncodeVars qry)

readMulti :: Maybe QueryArgs -> Tempodb (Maybe [SeriesData])
readMulti q = do
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http GET path
        auth

    (_,result) <- liftIO $ runRequest req Nothing
    return . A.decode $ fromStrict result

  where
    path = rootpath <> "/data" <> query
    query = case q of
        Nothing  -> mempty
        Just qry -> "?" <> (C8.pack $ urlEncodeVars qry)
