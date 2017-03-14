{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

module Kraken.Request.OHLC
       (
         OHLC(..),
       ) where

import qualified Kraken.Result.OHLC as R
import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data OHLC = OHLC
  { pair :: String
  , interval :: Maybe Integer
  , since :: Maybe String } deriving Generic
instance ToURLEncoded OHLC

instance Request OHLC where
  type Result OHLC = R.OHLC
  urlPart _ = "OHLC"
