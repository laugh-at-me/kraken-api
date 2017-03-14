{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

module Kraken.Request.Assets
       (
         Assets(..)
       ) where

import qualified Kraken.Result.Assets as R
import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Assets = Assets
  { info :: Maybe String
  , aclass :: Maybe String
  , asset :: Maybe [String] } deriving Generic
instance ToURLEncoded Assets

instance Request Assets where
  type Result Assets = R.Assets
  urlPart _ = "Assets"

