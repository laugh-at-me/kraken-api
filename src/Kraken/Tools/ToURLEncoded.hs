{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances #-}

module Kraken.Tools.ToURLEncoded (ToURLEncoded(encode)) where

import GHC.Generics
import Data.URLEncoded
import Control.Monad

class ToURLEncoded a where
  encode :: a -> URLEncoded
  default encode :: (Generic a, GToURLEncoded (Rep a)) => a -> URLEncoded
  encode = gencode . from
 
class GToURLEncoded f where
  gencode :: f a -> URLEncoded
 
instance (GToURLEncoded a, GToURLEncoded b) => GToURLEncoded (a :*: b) where
  gencode (a :*: b) = gencode a %& gencode b

instance (GToURLEncoded a) => GToURLEncoded (M1 i c a) where
  gencode (M1 a) = gencode a

instance GToURLEncoded U1 where
  gencode U1 = empty

instance {-# OVERLAPPING #-} (Selector c, URLShow a) => GToURLEncoded (M1 S c (K1 i (Maybe a))) where
  gencode m1@(M1 (K1 a)) = selName m1 %=? a

instance {-# OVERLAPPING #-} (Selector c, URLShow a) => GToURLEncoded (M1 S c (K1 i a)) where
  gencode m1@(M1 (K1 a)) = selName m1 %=? Just (urlShow a)
