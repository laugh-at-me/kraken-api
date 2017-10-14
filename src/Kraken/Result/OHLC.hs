{-# LANGUAGE OverloadedStrings #-}

module Kraken.Result.OHLC
       (
         OHLC(..),
         OHLCValue(..),
       ) where

import Kraken.Result
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import Control.Monad
import Data.Text
import Data.Aeson
import GHC.Generics

data OHLCTuple = OHLCTuple Int Double Double Double Double Double Double Int

data OHLCValue = OHLCValue
  { open :: Double
  , high :: Double
  , low :: Double
  , close :: Double
  , vwap :: Double
  , volume :: Double
  , count :: Int
  } deriving Show

data OHLC = OHLC
  { last :: Int
  , values :: M.Map String (M.Map Int OHLCValue) } deriving Show

instance FromJSON OHLCTuple where
  parseJSON = withArray "OHLC tuple" $ \a ->
    case V.toList a of
      [t, o, h, l, c, vw, vol, co] -> OHLCTuple <$> parseJSON t <*> parseQuotedDouble o <*> parseQuotedDouble h <*> parseQuotedDouble l <*> parseQuotedDouble c <*> parseQuotedDouble vw <*> parseQuotedDouble vol <*> parseJSON co
      _ -> fail "[Int, 6 String's, Int] expected"
    where
      parseQuotedDouble = parseJSON >=> (return . read)

instance FromJSON OHLC where
  parseJSON = withObject "OHLC result" $ \o ->
    OHLC <$> o .: "last" <*>
    (
      do
        rest <- parseJSON $ Object $ HM.delete "last" o
        return $ M.map (\a ->
                           Prelude.foldr (\(OHLCTuple t o h l c vw vol co) r ->
                                            M.insert t (OHLCValue o h l c vw vol co) r) M.empty (a::[OHLCTuple])) rest
    )

instance Kraken.Result.Result OHLC
