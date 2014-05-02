module ModelTypes where

-- We need this to be in a separate module to get 
-- through GHC's stage restriction for Template Haskell

import Prelude
import Database.Persist.Sql
import Database.Persist.TH

-- Internally, store everything in grams
newtype Weight = Kilograms {unKilo :: Int}
  deriving (Read, Show, Eq, Enum, Ord, Num, Real, Integral, PersistField, PersistFieldSql)

data WeightPref = Kg | Lbs
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField("WeightPref")


-- Maybe use this later, for things like pullups. How to solve
-- issue with things like weighted pull-ups?
data ExKind = WithWeight | NoWeight
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField("ExKind")
