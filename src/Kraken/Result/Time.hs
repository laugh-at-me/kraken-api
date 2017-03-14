{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Kraken.Result.Time
       (
         Time(..),
       ) where

import Kraken.Result
import Data.Aeson
import GHC.Generics

data Time = Time
  { unixtime :: Int
  , rfc1123 :: String
  } deriving (Show, Generic)

instance FromJSON Time
instance Kraken.Result.Result Time
