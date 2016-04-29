{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, PackageImports #-}

module Kraken () where

import qualified Kraken.Request as Request
import qualified Kraken.Result as Result
import qualified Kraken.Response as Response
import qualified Kraken.Tools.ToURLEncoded as URL
import qualified Data.ByteString.Base64 as Base64
import qualified "cryptohash" Crypto.Hash as CryptoHash
import qualified Data.ByteString as B
import System.IO
import Network.HTTP.Conduit
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.URLEncoded
import Data.Map
import Data.String
import Data.Byteable
import Data.Time.Clock.POSIX

public :: (Request.Request a, FromJSON b) => a -> IO (Either [String] b)
public request = do
  manager <- liftIO $ newManager tlsManagerSettings
  req' <- liftIO $ parseUrl ("https://api.kraken.com/0/public/" ++ (Request.urlPart request))
  req <- return (req' { method = "POST", requestBody = RequestBodyBS (fromString $ export $ URL.encode request)})
  res <- httpLbs req manager
  case (eitherDecode $ responseBody res) of
    Left error -> return (Left [error])
    Right response -> return
      (
        if Prelude.null e then
           case Response.result response of
             Nothing -> Left []
             Just r -> Right r
        else Left e
      )
      where e = (Response.error response)

private :: (Request.Request a, FromJSON b) => String -> String -> a -> IO (Either [String] b)
private apiKey privateKey request = do
  privateKey' <- return (Base64.decode (fromString privateKey))
  case privateKey' of
    Left e -> return $ Left [e]
    Right private ->
      let
        path = "/0/private/" ++ (Request.urlPart request)
        api = fromString (urlShow apiKey)
      in
        do
          nonce <- show `fmap` (((round . (1000*)) `fmap` getPOSIXTime)::IO Integer)
          postData <- return (export $ (URL.encode request %& ("nonce"::String) %= nonce))
          hash <- return $ toBytes ((CryptoHash.hash $ fromString $ nonce ++ postData)::(CryptoHash.Digest CryptoHash.SHA256))
          signature <- return $ Base64.encode $ toBytes $ CryptoHash.hmacGetDigest (((CryptoHash.hmac private $ B.concat [fromString path, hash]))::(CryptoHash.HMAC CryptoHash.SHA512))
          headers <- return [("API-Sign", signature), ("API-Key", api)]
          manager <- liftIO $ newManager tlsManagerSettings
          req' <- liftIO $ parseUrl $ "https://api.kraken.com" ++ path
          req <- return (req' { method = "POST", requestBody = RequestBodyBS $ fromString postData, requestHeaders = headers })
          res <- httpLbs req manager
          case (eitherDecode $ responseBody res) of
            Left error -> return (Left [error])
            Right response -> return
              (
                if Prelude.null e then
                  case Response.result response of
                    Nothing -> Left []
                    Just r -> Right r
                else Left e
              )
              where e = (Response.error response)

main :: IO ()
main = do
  ((public $ Request.Assets { Request.info = Nothing, Request.aclass = Nothing, Request.asset = Nothing })::(IO (Either [String] Result.Assets))) >>= print
  ((public $ Request.Time)::(IO (Either [String] (Result.Time)))) >>= print
  ((private "api" "private" $ Request.Balance)::(IO (Either [String] Result.Balance))) >>= print
