{-# LANGUAGE DeriveGeneric #-}

import ToURLEncoded
import Data.URLEncoded
import GHC.Generics

data Test = Test
  { elephant :: Bool
  , message  :: String
  , number   :: Int
  , maybe    :: Maybe Int
  , maybe2    :: Maybe Int} deriving Generic

instance ToURLEncoded Test

main :: IO ()
main = print $ export $ encode $ Test True "Yololo World!" 42 Nothing (Just 5)
