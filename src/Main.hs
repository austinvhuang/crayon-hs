{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BS
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.Client

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML Text where
  mimeRender _ = BSC.pack . T.unpack

instance MimeUnrender HTML Text where
  mimeUnrender _ bs = Right $ pack $ BSC.unpack bs

-- TODO: handle this decoding properly
instance MimeUnrender HTML Experiments where
  mimeUnrender _ bs = Right $ Experiments [pack $ BSC.unpack bs]

data Version = Version {
  version :: Text
  } deriving (Show, Generic)

instance ToJSON Version
instance FromJSON Version

data Experiments = Experiments {
  experiments :: [Text]
  } deriving (Show, Generic)

instance ToJSON Experiments
instance FromJSON Experiments

type API =
  Get '[HTML] Text
  :<|> "data" :> Get '[HTML] Experiments
  :<|> "data" :> QueryParam "xp" Text :> Get '[HTML] Text

api :: Proxy API
api = Proxy

clientVersion :<|> clientExperiments :<|> clientScalars = client api

main :: IO ()
main = do
  testVersion >> putStrLn ""
  testExperiments >> putStrLn ""
  testScalars (Just "test_experiment") >> putStrLn ""

defaultEnv = do
  manager <- (newManager defaultManagerSettings)
  pure (ClientEnv manager (BaseUrl Http "localhost" 8889 ""))

handleResult res =
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right x -> print x

testVersion :: IO ()
testVersion = do
  res <- (defaultEnv >>= \env -> (runClientM clientVersion) env)
  handleResult res

testExperiments :: IO ()
testExperiments = do
  res <- defaultEnv >>= \env -> (runClientM clientExperiments) env
  handleResult res

testScalars expName = do
  res <- defaultEnv >>= \env -> (runClientM $ clientScalars expName) env
  handleResult res
