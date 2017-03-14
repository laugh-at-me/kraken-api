{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TypeFamilies, FlexibleContexts #-}

module Kraken.Request
       (
         Request(..)
       ) where

import Kraken.Result
import Kraken.Tools.ToURLEncoded
import Data.URLEncoded
import GHC.Generics

class (ToURLEncoded a, Kraken.Result.Result (Kraken.Request.Result a)) => Request a where
  type Result a
  urlPart :: a -> String

instance URLShow [String] where
  urlShow = urlShow . Prelude.foldr1 (\ x y -> concat [x, ",", y])

instance URLShow Double where
  urlShow = show
