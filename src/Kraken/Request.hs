{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Kraken.Request
       (
         Request(..)
       ) where

import Kraken.Tools.ToURLEncoded
import Data.URLEncoded
import GHC.Generics

class ToURLEncoded a => Request a where
  urlPart :: a -> String

instance URLShow [String] where
  urlShow = urlShow . Prelude.foldr1 (\ x y -> concat [x, ",", y])
