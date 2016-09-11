{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Kraken.Request.Balance
       (
         Balance(..)
       ) where

import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Balance = Balance deriving Generic
instance ToURLEncoded Balance

instance Request Balance where
  urlPart _ = "Balance"

