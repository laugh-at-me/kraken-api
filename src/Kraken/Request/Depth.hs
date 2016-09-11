{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Kraken.Request.Depth
       (
         Depth(..)
       ) where

import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Depth = Depth
  { pair :: String
  , count :: Maybe Integer } deriving Generic
instance ToURLEncoded Depth

instance Request Depth where
  urlPart _ = "Depth"

