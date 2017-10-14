{-# LANGUAGE OverloadedStrings, FlexibleInstances,
  MultiParamTypeClasses, ScopedTypeVariables, PackageImports #-}
module Kraken (public, private) where

import qualified Kraken.Request as Request
import qualified Kraken.Result as Result
import qualified Kraken.Response as Response
import qualified Kraken.Tools.ToURLEncoded as URL
import qualified Data.ByteString.Base64 as Base64
import qualified "cryptohash" Crypto.Hash as CryptoHash
import qualified Data.ByteString as B
import Data.List
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

public :: (Request.Request a) => a -> IO (Request.Result a)
public request = do
  manager <- liftIO $ newManager tlsManagerSettings
  req' <- liftIO $ parseUrlThrow ("https://api.kraken.com/0/public/" ++ Request.urlPart request)
  let req = req' { method = "POST", requestBody = RequestBodyBS (fromString $ export $ URL.encode request)}
  res <- httpLbs req manager
  case eitherDecode $ responseBody res of
    Left error -> fail (intercalate "\n" [error])
    Right response ->
        if Prelude.null e then
           case Response.result response of
             Nothing -> fail "Empty result"
             Just r -> return r
        else fail (intercalate "\n" e)
      where e = Response.error response

private :: (Request.Request a) => String -> String -> a -> IO (Request.Result a)
private apiKey privateKey request = do
  let privateKey' = Base64.decode (fromString privateKey)
  case privateKey' of
    Left e -> fail e
    Right private ->
      let
        path = "/0/private/" ++ Request.urlPart request
        api = fromString (urlShow apiKey)
      in
        do
          nonce <- show `fmap` (((round . (1000*)) `fmap` getPOSIXTime)::IO Integer)
          let postData = export (URL.encode request %& ("nonce"::String) %= nonce)
          let hash = toBytes ((CryptoHash.hash $ fromString $ nonce ++ postData)::(CryptoHash.Digest CryptoHash.SHA256))
          let signature = Base64.encode $ toBytes $ CryptoHash.hmacGetDigest ((CryptoHash.hmac private $ B.concat [fromString path, hash])::(CryptoHash.HMAC CryptoHash.SHA512))
          let headers = [("API-Sign", signature), ("API-Key", api)]
          manager <- liftIO $ newManager tlsManagerSettings
          req' <- liftIO $ parseUrlThrow $ "https://api.kraken.com" ++ path
          let req = req' { method = "POST", requestBody = RequestBodyBS $ fromString postData, requestHeaders = headers }
          res <- httpLbs req manager
          case eitherDecode $ responseBody res of
            Left error -> fail error
            Right response ->
                if Prelude.null e then
                  case Response.result response of
                    Nothing -> fail "Empty result"
                    Just r -> return r
                else fail (intercalate "\n" e)
              where e = Response.error response
