{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Kraken.Result.Assets
       (
         Asset(..),
         Assets(..),
       ) where

import Kraken.Result
import qualified Data.Map as M
import Data.Aeson
import GHC.Generics

{-# ANN Asset ("HLint: ignore Use camelCase" :: String) #-}
data Asset = Asset
  { altname :: String
  , aclass :: String
  , decimals :: Integer
  , display_decimals :: Integer } deriving (Show, Generic)

instance FromJSON Asset

type Assets = M.Map String Asset

instance Kraken.Result.Result Assets
