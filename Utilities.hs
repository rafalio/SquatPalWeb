{-# LANGUAGE OverloadedStrings #-}

module Utilities where

import Prelude
import Yesod
import Control.Arrow
import qualified Data.Text as T


enumTypeTuples :: (Show a, Enum a, Bounded a) => [(T.Text,a)]
enumTypeTuples = map (T.pack . show &&& id) [minBound..maxBound]

entityVal2 = map f2
    where f2 (a,b) = (entityVal a, entityVal b)

entityVal3 = map f3
    where f3 (a,b,c) = (entityVal a, entityVal b, entityVal c)


