{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Kraken.Result
       (
         Kraken.Result.Result(..)
       ) where

import Data.Aeson

class FromJSON a => Result a
