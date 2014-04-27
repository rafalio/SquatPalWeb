module ModelTypes where

-- We need this to be in a separate module to get 
-- through GHC's stage restriction for Template Haskell

import Prelude
import Database.Persist.Sql

-- Internally, store everything in grams
newtype Weight = Grams Int
  deriving (Read, Show, Eq, PersistField, PersistFieldSql)
