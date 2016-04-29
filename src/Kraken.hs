{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module Kraken () where

import qualified Kraken.Request as Request
import qualified Kraken.Result as Result
import qualified Kraken.Response as Response
import qualified Kraken.Tools.ToURLEncoded as URL
import System.IO
import Network.HTTP.Conduit
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.URLEncoded
import Data.Map
import Data.String

kraken :: (Request.Request a, FromJSON b) => a -> IO (Either [String] b)
kraken request = do
  manager <- liftIO $ newManager tlsManagerSettings
  req' <- liftIO $ parseUrl ("https://api.kraken.com/0/public/" ++ (Request.urlPart request))
  req <- return (req' { requestBody = RequestBodyBS (fromString $ export $ URL.encode request)})
  res <- httpLbs req manager
  case (eitherDecode $ responseBody res) of
    Left error -> return (Left [error])
    Right response -> return
      (
        if Prelude.null e then Right (Response.result response)
        else Left e
      )
      where e = (Response.error response)

main :: IO ()
main = do
  ((kraken $ Request.Assets { Request.info = Nothing, Request.aclass = Nothing, Request.asset = Nothing })::(IO (Either [String] (Map String Result.Assets)))) >>= print
  ((kraken $ Request.Time)::(IO (Either [String] (Result.Time)))) >>= print
