-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Tempodb.Methods
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- Initialize a BasicAuth value:
-- > let a = BasicAuth (ApiKey "mykey") (ApiSec "mysecret")
--
-- Then use that with `runTempoDB` to run a single action or do-syntax
-- sequenced actions if you want to run multiple operations.
----------------------------------------------------------------------------
module Database.Tempodb.Methods
(
-- * Operate on Series Objects
  module Database.Tempodb.Methods.Series
-- * Read from Series
, module Database.Tempodb.Methods.Read
-- * Create or Write to Series
, module Database.Tempodb.Methods.Write
-- * Delete data in a Series
-- ** (not implemented)
-- * Single Datapoint Operations
-- ** (not implemented)
-- * Incrementer
-- ** (not implemented)
)
where

import           Database.Tempodb.Methods.Read
import           Database.Tempodb.Methods.Series
import           Database.Tempodb.Methods.Write
--import Database.Tempodb.Methods.Increment
--import Database.Tempodb.Methods.Single
--import Database.Tempodb.Methods.Delete
