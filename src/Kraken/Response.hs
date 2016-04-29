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
  , result :: Maybe a
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Response a)
