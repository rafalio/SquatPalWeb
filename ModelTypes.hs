{-# LANGUAGE DeriveGeneric #-}

module ModelTypes where

-- We need this to be in a separate module to get 
-- through GHC's stage restriction for Template Haskell

import Prelude
import Database.Persist.Sql
import Database.Persist.TH

import Data.Aeson
import GHC.Generics

import Yesod.Form.Fields ( Textarea(..), unTextarea)
import Control.Monad



-- Internally, store everything in kilograms as a double
newtype Weight = Kilograms {unKilo :: Double}
  deriving (Read, Show, Eq, Ord, Num, Real, PersistField, PersistFieldSql, Generic)

instance ToJSON Weight
instance FromJSON Weight

data WeightPref = Kg | Lbs
    deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField("WeightPref")

instance ToJSON WeightPref
instance FromJSON WeightPref

-- Maybe use this later, for things like pullups. How to solve
-- issue with things like weighted pull-ups?
data ExKind = WithWeight | NoWeight
    deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField("ExKind")

instance ToJSON ExKind
instance FromJSON ExKind

instance ToJSON Textarea where
    toJSON t = String $ unTextarea t

instance FromJSON Textarea where
    parseJSON (String t) = return $ (Textarea t)
    parseJSON _          = mzero
