{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Kraken.Request.OHLC
       (
         OHLC(..),
       ) where

import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data OHLC = OHLC
  { pair :: String
  , interval :: Maybe Integer
  , since :: Maybe String } deriving Generic
instance ToURLEncoded OHLC

instance Request OHLC where
  urlPart _ = "OHLC"
