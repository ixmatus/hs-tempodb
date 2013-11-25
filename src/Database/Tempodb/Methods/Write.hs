{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Methods.Write
( writeOne
, writeBulk
, writeMulti
, SeriesDataWrite(..)
, Bulk(..)
)
where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson             as A
import           Data.Aeson.Types       (Parser)
import           Data.ByteString.Char8  as C8
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Time
import qualified Database.Tempodb.Types as T
import           Database.Tempodb.Util
import           Network.Http.Client
import           System.Locale

data SeriesDataWrite = SeriesDataWrite
    { uid    :: T.IdOrKey
    , tstamp :: Maybe UTCTime
    , value  :: Double
    } deriving (Show, Eq, Ord)

data Bulk = Bulk
    { timestamp :: UTCTime
    , values    :: [SeriesDataWrite]
    } deriving (Show, Eq, Ord)

instance FromJSON Bulk where
    parseJSON (Object o) = Bulk     <$>
                           o .: "t" <*>
                           o .: "data"

instance ToJSON Bulk where
    toJSON (Bulk t v) = object
        [ "t"    .= t
        , "data" .= v
        ]

instance FromJSON SeriesDataWrite where
    parseJSON = parseSeriesBulk

parseSeriesBulk :: Value -> Parser SeriesDataWrite
parseSeriesBulk v = do
    case v of
        Object o -> do
            -- Is the ID there? Or is the key element there?
            kid <- (o .:? "id") >>= isUID o
            ts  <- o .:? "t"
            val <- o .: "v"
            return $ SeriesDataWrite kid ts val
        _ -> mzero
  where
    isUID o i = case i of
        Nothing    -> (o .:? "key") >>= isKEY
        Just idval -> return $ T.SeriesId idval
    isKEY k = case k of
        Nothing    -> fail ((show v) ++ " is not a valid object.")
        Just idkey -> return $ T.SeriesKey idkey

instance ToJSON SeriesDataWrite where
    toJSON = buildSeriesBulk

buildSeriesBulk :: SeriesDataWrite -> Value
buildSeriesBulk (SeriesDataWrite i t v) = object
    [ eid
    , ts
    , "v" .= v
    ]
  where
    ts  = case t of
        Nothing -> ("","")

        Just tv -> ("t", String . T.pack $ formatTime defaultTimeLocale "%FT%H:%M:%S%Q%z" tv)
    eid = case i of
        T.SeriesId idv -> "id" .= idv
        T.SeriesKey kv -> "key".= kv

writeOne :: T.IdOrKey -> T.Data -> T.Tempodb Bool
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

writeBulk :: Bulk -> T.Tempodb Bool
writeBulk d = do
    let postData = toStrict $ A.encode d
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http POST "/data"
        setContentLength . fromIntegral $ C8.length postData
        auth

    liftIO . runRequest req $ Just postData

    return True

writeMulti :: [SeriesDataWrite] -> T.Tempodb (Either (Maybe [SeriesDataWrite]) Bool)
writeMulti d = do
    let postData = toStrict $ A.encode d
    auth <- ask
    req  <- liftIO . buildRequest $ do
        http POST "/multi"
        setContentLength . fromIntegral $ C8.length postData
        auth

    (s, result) <- liftIO . runRequest req $ Just postData
    return $ (
        if s < 207
           then Right True
           else Left . A.decode $ fromStrict result)
