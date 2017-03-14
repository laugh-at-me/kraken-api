{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeFamilies #-}

module Kraken.Request.Depth
       (
         Depth(..)
       ) where

import qualified Kraken.Result.Depth as R
import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Depth = Depth
  { pair :: String
  , count :: Maybe Integer } deriving Generic
instance ToURLEncoded Depth

instance Request Depth where
  type Result Depth = R.Depth
  urlPart _ = "Depth"

