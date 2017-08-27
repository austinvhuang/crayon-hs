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
import Data.Aeson.Parser
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.Client

{- Types and Decoding/Encoding -}

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML Text where
  mimeRender _ = BSC.pack . T.unpack

instance MimeUnrender HTML Text where
  mimeUnrender _ bs = Right $ pack $ BSC.unpack bs

instance MimeUnrender HTML Version where
  mimeUnrender _ bs = Right $ Version $ pack $ BSC.unpack bs

instance MimeUnrender HTML Experiments where
  -- TODO: handle decoding of list of experiments
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
  -- for reference see https://github.com/torrvision/crayon/blob/master/client/python/pycrayon/crayon.py
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

type ScalarAPI =
  "data" :> "scalars"
  :> QueryParam "xp" Text :> QueryParam "name" Text
  :> ReqBody '[JSON] [Double]
  :> Post '[HTML] Text

  :<|> "data" :> "scalars"
  :> QueryParam "xp" Text
  :> QueryParam "name" Text
  :> Get '[JSON] [Double]

type HistogramAPI =

  "data" :> "histograms"
  :> QueryParam "xp" Text :> QueryParam "name" Text :> QueryParam "toBuild" Bool
  :> ReqBody '[JSON] [Double] -- TODO : vary type depending on toBuild value
  :> Post '[HTML] Text

  :<|> "data" :> "histograms"
  :> QueryParam "xp" Text :> QueryParam "name" Text
  :> Get '[JSON] Text -- TODO - build type for histogram json

type API = ManagementAPI :<|> ScalarAPI :<|> HistogramAPI

clientVersion
  :<|> clientExperiments
  :<|> clientListScalars
  :<|> clientAddExperiment
  :<|> clientDeleteExperiment = client (Proxy :: Proxy ManagementAPI)

clientAddScalar :<|> clientGetScalar = client (Proxy :: Proxy ScalarAPI)

clientAddHistogram :<|> clientGetHistogram = client (Proxy :: Proxy HistogramAPI)

{- Test Client Functions -}

defaultEnv = do
  manager <- (newManager defaultManagerSettings)
  pure (ClientEnv manager (BaseUrl Http "localhost" 8889 ""))

handleResult res =
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right x -> print x

runTest clientFun = do
 res <- (defaultEnv >>= \env -> (runClientM clientFun) env)
 handleResult res

testVersion :: IO ()
testVersion = runTest clientVersion

testExperiments :: IO ()
testExperiments = runTest clientExperiments

testListScalars :: Text -> IO ()
testListScalars expName = runTest $ clientListScalars (Just expName)

testAddExperiment :: Text -> IO ()
testAddExperiment expName = runTest $ clientAddExperiment expName

testDeleteExperiment :: Text -> IO ()
testDeleteExperiment expName = runTest $ clientDeleteExperiment (Just expName)

testAddScalar :: Text -> Text -> Scalar -> IO ()
testAddScalar expName metricName scalar =
  runTest $ clientAddScalar (Just expName) (Just metricName) val
  where val = [wallTime scalar, step scalar, Main.value scalar]

testGetScalar :: Text -> Text -> IO ()
testGetScalar expName metricName =
  runTest $ clientGetScalar (Just expName) (Just metricName)

main :: IO ()
main = do
  putStrLn "Version"
  testVersion >> putStrLn ""

  putStrLn "List Experiments"
  testExperiments >> putStrLn ""

  putStrLn "Add Experiment"
  testAddExperiment "test_experiment" >> putStrLn ""

  putStrLn "List Experiments (again)"
  -- TODO : this doesn't list the added experiment, why?
  testExperiments >> putStrLn ""

  putStrLn "List Scalars"
  testListScalars "test_experiment" >> putStrLn ""

  putStrLn "Add Scalar"
  -- TODO: fix 500 status code
  testAddScalar "test_experiment" "metricFoo" (Scalar (-1.0) (-1.0) 2.0)
    >> putStrLn ""

  putStrLn "Get Scalar"
  testGetScalar "test_experiment" "metricFoo" >> putStrLn ""

  putStrLn "Delete Experiment"
  testDeleteExperiment "test_experiment" >> putStrLn ""
