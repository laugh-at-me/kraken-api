{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Kraken.Request
       (
         Time(..),
         Assets(..),
         Balance(..),
         Request(..),
       ) where

import qualified Kraken.Result as Result
import Kraken.Tools.ToURLEncoded
import Data.Map
import Data.URLEncoded
import Data.Aeson
import GHC.Generics

data Proxy a = Proxy

class ToURLEncoded a => Request a where
  urlPart :: a -> String

instance URLShow [String] where
  urlShow = urlShow . Prelude.foldr1 (\ x y -> concat [x, ",", y])

data Time = Time deriving Generic
instance ToURLEncoded Time

data Assets = Assets
  { info :: Maybe String
  , aclass :: Maybe String
  , asset :: Maybe [String] } deriving Generic
instance ToURLEncoded Assets

data Balance = Balance deriving Generic
instance ToURLEncoded Balance

instance Request Assets where
  urlPart _ = "Assets"

instance Request Time where
  urlPart _ = "Time"

instance Request Balance where
  urlPart _ = "Balance"
