{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

module Kraken.Request.AddOrder
       (
         AddOrder(..),
         Type(..)
       ) where

import qualified Kraken.Result.AddOrder as R
import Kraken.Request
import Kraken.Tools.ToURLEncoded
import Data.URLEncoded
import Data.Char
import GHC.Generics

data Type = Buy | Sell
  deriving Show

instance URLShow Type where
  urlShow = toSnake . show

data OrderType = Market |
                 Limit |
                 StopLoss |
                 TakeProfit |
                 StopLossProfit |
                 StopLossProfitLimit |
                 StopLossLimit |
                 TakeProfitLimit |
                 TrailingStop |
                 TrailingStopLimit |
                 StopLossAndLimit |
                 SettlePosition
  deriving Show

instance URLShow OrderType where
  urlShow = toSnake . show

data AddOrder = AddOrder {
  pair :: String,
  type_ :: Type,
  orderType :: OrderType,
  price :: Maybe Double,
  price2 :: Maybe Double,
  leverage :: Maybe Int  
  } deriving Generic

splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case break p s' of
      (b', [])     -> [ m:b' ]
      (b', (x:xs)) -> ( m:b' ) : go x xs
  in case break p s of
    (b,  [])    -> [ b ]
    ([], (h:t)) -> go h t
    (b,  (h:t)) -> b : go h t

downcase :: String -> String
downcase = map toLower

toSnake :: String -> String
toSnake = downcase . concat . dashes . splitR isUpper
  where
    dashes :: [String] -> [String]
    dashes [] = []
    dashes (h:t) = h : map ('-':) t

instance ToURLEncoded AddOrder

instance Request AddOrder where
  type Result AddOrder = R.AddOrder
  urlPart _ = "AddOrder"

