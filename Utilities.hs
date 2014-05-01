{-# LANGUAGE OverloadedStrings #-}

module Utilities where

import Prelude
import Yesod
import Control.Arrow
import qualified Data.Text as T


enumTypeTuples :: (Show a, Enum a, Bounded a) => [(T.Text,a)]
enumTypeTuples = map (T.pack . show &&& id) [minBound..maxBound]
