{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

module Kraken.Request.Time
       (
         Time(..)
       ) where

import qualified Kraken.Result.Time as R
import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Time = Time deriving Generic
instance ToURLEncoded Time

instance Request Time where
  type Result Time = R.Time
  urlPart _ = "Time"
