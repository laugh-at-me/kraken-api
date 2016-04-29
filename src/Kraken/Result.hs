{-# LANGUAGE DeriveGeneric #-}

module Kraken.Result
       (
         Time(..),
         Asset(..),
         Assets(..),
         Balance(..),
         Kraken.Result.Result(..)
       ) where

import Data.Map
import Data.Text
import Data.Aeson
import GHC.Generics

data Time = Time
  { unixtime :: Integer
  , rfc1123 :: String
  } deriving (Show, Generic)

instance FromJSON Time

data Asset = Asset
  { altname :: String
  , aclass :: String
  , decimals :: Integer
  , display_decimals :: Integer } deriving (Show, Generic)

instance FromJSON Asset

type Assets = Map String Asset

type Balance = Map String String

class FromJSON a => Result a

instance Kraken.Result.Result Asset
instance Kraken.Result.Result Time
