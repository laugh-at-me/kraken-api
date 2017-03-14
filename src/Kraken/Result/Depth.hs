{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Kraken.Result.Depth
       (
         Depth(..),
         DepthValue(..),
         DepthElementValue(..),
       ) where

import Kraken.Result
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad
import Data.Text
import Data.Aeson
import GHC.Generics

data DepthElementTuple = DepthElementTuple Double Double Integer

data DepthElementValue = DepthElementValue
  { price :: Double
  , depthVolume :: Double
  , timestamp :: Integer } deriving Show

data DepthValue = DepthValue
  { asks :: [ DepthElementValue ]
  , bids :: [ DepthElementValue ] } deriving (Show, Generic)

newtype Depth = Depth (M.Map String DepthValue) deriving (Show, Generic)

instance FromJSON DepthElementTuple where
  parseJSON = withArray "Depth element tuple" $ \a ->
    case V.toList a of
      [p, v, t] -> DepthElementTuple <$> parseQuotedDouble p <*> parseQuotedDouble v <*> parseJSON t
      _ -> fail "[2 String's, Int] expected"
    where
      parseQuotedDouble = parseJSON >=> (return . read)

instance FromJSON DepthElementValue where
  parseJSON o =
    do
      (DepthElementTuple p v t) <- parseJSON o
      return $ DepthElementValue p v t

instance FromJSON DepthValue
instance FromJSON Depth
instance Kraken.Result.Result Depth
