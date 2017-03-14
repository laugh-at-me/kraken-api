{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

module Kraken.Request.Balance
       (
         Balance(..)
       ) where

import qualified Kraken.Result.Balance as R
import Kraken.Request
import Kraken.Tools.ToURLEncoded
import GHC.Generics

data Balance = Balance deriving Generic
instance ToURLEncoded Balance

instance Request Balance where
  type Result Balance = R.Balance
  urlPart _ = "Balance"

