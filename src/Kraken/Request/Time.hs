{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Kraken.Request.Time
       (
         Time(..)
       ) where

import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Time = Time deriving Generic
instance ToURLEncoded Time

instance Request Time where
  urlPart _ = "Time"
