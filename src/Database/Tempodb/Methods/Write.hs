{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Methods.Write
( writeOne
, writeBulk
, writeMulti
)
where

import           Control.Monad.Reader
import           Data.Aeson             as A
import           Data.ByteString.Char8  as C8
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Int
import           Data.Monoid
import           Database.Tempodb.Types as T
import           Database.Tempodb.Util
import           Network.Http.Client

writeOne :: IdOrKey -> Data Int64 -> Tempodb Bool
writeOne q d = do
    let postdata = toStrict $ A.encode d
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http POST path
        setContentLength . fromIntegral $ C8.length postdata
        auth

    liftIO . runRequest req $ Just postdata

    -- It will either throw an exception or return a Bool
    return True
  where
    ident (T.SeriesId i)   = "/id/" <> i
    ident (T.SeriesKey k) = "/key/" <> k
    path = rootpath <> "/series" <> (ident q)

writeBulk :: Bulk Int64 -> Tempodb Bool
writeBulk d = do
    let postData = toStrict $ A.encode d
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http POST (rootpath <> "/data/")
        setContentLength . fromIntegral $ C8.length postData
        auth

    liftIO . runRequest req $ Just postData

    return True

writeMulti :: [Data Int64] -> T.Tempodb (Either (Maybe [Data Int64]) Bool)
writeMulti d = do
    let postData = toStrict $ A.encode d
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http POST (rootpath <> "/multi/")
        setContentLength . fromIntegral $ C8.length postData
        auth

    (s, result) <- liftIO . runRequest req $ Just postData
    return $ (
        if s < 207
           then Right True
           else Left . A.decode $ fromStrict result)
