{-# LANGUAGE OverloadedStrings #-}

module Database.Tempodb.Methods.Read
( readOne
, readMulti
)
where

import           Control.Monad.Reader
import           Data.Aeson             as A
import           Data.ByteString.Char8  as C8
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Monoid
import           Database.Tempodb.Types
import           Database.Tempodb.Util
import           Network.HTTP.Base      (urlEncodeVars)
import           Network.Http.Client

readOne :: IdOrKey -> Tempodb (Maybe Series)
