{-# LANGUAGE DeriveGeneric #-}

module Kraken.Result
       (
         Time(..),
         Assets(..),
         Kraken.Result.Result(..)
       ) where

import Data.Text
import Data.Aeson
import GHC.Generics

data Time = Time
  { unixtime :: Integer
  , rfc1123 :: String
  } deriving (Show, Generic)

instance FromJSON Time

data Assets = Assets
  { altname :: String
  , aclass :: String
  , decimals :: Integer
  , display_decimals :: Integer } deriving (Show, Generic)

instance FromJSON Assets

class FromJSON a => Result a

instance Kraken.Result.Result Assets
instance Kraken.Result.Result Time
