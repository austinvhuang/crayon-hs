{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BS
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Aeson
import Data.Aeson.Parser
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.Client

import           Data.Aeson.Types                 (parseEither)
import Data.Attoparsec.ByteString.Char8 (endOfInput, parseOnly,
                                          skipSpace, (<?>))
import           Data.String.Conversions          (cs)

{- Format parsing -}

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML Text where
  mimeRender _ = BSC.pack . T.unpack

instance MimeUnrender HTML Text where
  mimeUnrender _ bs = Right $ pack $ BSC.unpack bs

instance MimeUnrender HTML Version where
  mimeUnrender _ bs = Right $ Version $ pack $ BSC.unpack bs

-- TODO: handle this decoding
instance MimeUnrender HTML Experiments where
  mimeUnrender _ bs = Right $ Experiments [pack $ BSC.unpack bs]

data Version = Version {
  version :: Text
  } deriving (Show, Generic)

data Experiments = Experiments {
  experiments :: [Text]
  } deriving (Show, Generic)

instance FromJSON Experiments

data Scalar = Scalar {
  -- TODO : determine expected types for Scalar fields
  wallTime :: Double,
  step :: Double,
  value :: Double
  } deriving (Show, Generic)

instance FromJSON Scalar
instance ToJSON Scalar

{- Specify Crayon and Generate Client Functions -}

type ManagementAPI =
  Get '[HTML] Version
  :<|> "data" :> Get '[HTML] Experiments
  :<|> "data" :> QueryParam "xp" Text :> Get '[HTML] Text
  :<|> "data" :> ReqBody '[JSON] Text :> Post '[HTML] Text
  :<|> "data" :> QueryParam "xp" Text :> Delete '[HTML] Text

type ScalarAPI = "data" :> "scalars"
                 :> QueryParam "xp" Text
                 :> QueryParam "name" Text :> ReqBody '[JSON] [Double] :> Post '[HTML] Text
  :<|> "data" :> "scalars" :> QueryParam "xp" Text :> Get '[JSON] [Double]

type API = ManagementAPI :<|> ScalarAPI

clientVersion
  :<|> clientExperiments
  :<|> clientListScalars
  :<|> clientAddExperiment
  :<|> clientDeleteExperiment = client (Proxy :: Proxy ManagementAPI)

clientAddScalar :<|> clientGetScalar = client (Proxy :: Proxy ScalarAPI)

{- Test Client Functions -}

main :: IO ()
main = do
  putStrLn "Version"
  testVersion >> putStrLn ""

  putStrLn "List Experiments"
  testExperiments >> putStrLn ""

  putStrLn "Add Experiment"
  testAddExperiment "test_experiment" >> putStrLn ""

  putStrLn "List Experiments (again)"
  testExperiments >> putStrLn "" -- TODO : this doesn't list the added experiment, why?

  putStrLn "List Scalars" -- TODO: fix 500 status code
  testListScalars "test_experiment" >> putStrLn ""

  putStrLn "Add Scalar" -- TODO: fix 500 status code
  testAddScalar "test_experiment" "metricFoo" (Scalar (-1.0) (-1.0) 2.0) >> putStrLn ""

  putStrLn "Delete Experiment"
  testDeleteExperiment "test_experiment" >> putStrLn ""


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

testListScalars expName = do
  res <- defaultEnv >>= \env -> (runClientM $ clientListScalars (Just expName)) env
  handleResult res

testAddExperiment expName = do
  res <- defaultEnv >>= \env -> (runClientM $ clientAddExperiment expName) env
  handleResult res

testDeleteExperiment expName = do
  res <- defaultEnv >>= \env ->
    (runClientM $ clientDeleteExperiment (Just expName)) env
  handleResult res

testAddScalar expName metricName scalar = do
  let val = [wallTime scalar, step scalar, Main.value scalar]
  res <- defaultEnv >>= \env ->
    (runClientM $ clientAddScalar (Just expName) (Just metricName) val) env
  handleResult res

-- testGetScalar expName = do
--   res <- defaultEnv >>= \env -> (runClientM $ clientDeleteExperiment expName) env
--   handleResult res
