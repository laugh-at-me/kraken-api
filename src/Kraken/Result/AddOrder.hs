{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Kraken.Result.AddOrder
       (
         AddOrder(..)
       ) where

import Kraken.Result
import Data.Text
import Data.Aeson
import GHC.Generics

data Descr = Descr
  { order :: String
  , close :: Maybe String } deriving (Show, Generic)
    
data AddOrder = AddOrder
  { descr :: Descr
  , txid :: Maybe [Integer] } deriving (Show, Generic)

instance FromJSON Descr
instance FromJSON AddOrder

instance Kraken.Result.Result AddOrder
