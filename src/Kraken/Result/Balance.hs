{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Kraken.Result.Balance
       (
         Balance(..),
       ) where

import Kraken.Result
import qualified Data.Map as M

type Balance = M.Map String String

instance Kraken.Result.Result Balance
