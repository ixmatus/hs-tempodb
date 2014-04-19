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
(
-- * TempoDB Endpoint Methods
-- ** (the good stuff is in here)
  module Database.Tempodb.Methods
-- * Oooh Yeah, Types
, module Database.Tempodb.Types
) where

import           Database.Tempodb.Methods
import           Database.Tempodb.Types
