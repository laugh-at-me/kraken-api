{-# LANGUAGE DeriveGeneric #-}

module Kraken.Response
       (
         Response(..)
       ) where

import Data.Text
import Data.Aeson
import GHC.Generics

data Response a = Response
  { error :: [String]
  , result :: a
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Response a)
