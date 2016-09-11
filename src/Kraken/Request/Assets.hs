{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Kraken.Request.Assets
       (
         Assets(..)
       ) where

import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Assets = Assets
  { info :: Maybe String
  , aclass :: Maybe String
  , asset :: Maybe [String] } deriving Generic
instance ToURLEncoded Assets

instance Request Assets where
  urlPart _ = "Assets"

