-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Tempodb
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- Please familiarize yourself with the <https://tempo-db.com/docs/api/ TempoDB REST API>. The
-- `Database.Tempodb.Methods` mimic TempoDB's REST endpoints.
----------------------------------------------------------------------------
module Database.Tempodb
( module Database.Tempodb.Types
, module Database.Tempodb.Methods
) where

import           Database.Tempodb.Methods
import           Database.Tempodb.Types
