{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances, DeriveGeneric #-}
 
import GHC.Generics
import Data.URLEncoded

class ToURLEncodable a where
  prepare :: a -> [(String, String)]
 
  default prepare :: (Generic a, GToURLEncodable (Rep a)) => a -> [(String, String)]
  prepare x = zip n v 
    where (n, v) = gprepare . from $ x
 
class GToURLEncodable f where
  gprepare :: f a -> ([String], [String])
 
instance (GToURLEncodable a, GToURLEncodable b) => GToURLEncodable (a :*: b) where
  gprepare (a :*: b) = (an ++ bn, av ++ bv)
    where (an, av) = gprepare a
          (bn, bv) = gprepare b

instance (GToURLEncodable a) => GToURLEncodable (M1 D s a) where
  gprepare (M1 x) = gprepare x

instance (GToURLEncodable a) => GToURLEncodable (M1 C s a) where
  gprepare (M1 x) = gprepare x

instance (GToURLEncodable a, Selector s) => GToURLEncodable (M1 S s a) where
  gprepare m1@(M1 x) = (n ++ [selName m1], v)
    where (n, v) = gprepare x

instance {-# OVERLAPPING #-} (Show a) => GToURLEncodable (K1 i a) where
  gprepare (K1 x) = ([], [show x])

instance {-# OVERLAPPING #-} GToURLEncodable (K1 i String) where
  gprepare (K1 x) = ([], [x])

data Test = Test
  { elephant :: Bool
  , message  :: String
  , number   :: Int } deriving Generic

instance ToURLEncodable Test

main = do
  print $ prepare $ Test True "Yololo World!" 42
  print $ export $ importList $ prepare $ Test True "Yololo World!" 42
