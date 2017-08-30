{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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

instance MimeUnrender HTML Double where
  -- TODO: handle decoding of list of experiments
  -- mimeUnrender _ bs = Right $ Experiments [pack $ BSC.unpack bs]
  mimeUnrender _ bs = Right $ 1.0

-- [[Double]] here requires flexible instances
instance MimeUnrender HTML [(Double, Int, Double)] where
  -- TODO: handle decoding of list of experiments
  -- mimeUnrender _ bs = Right $ Experiments [pack $ BSC.unpack bs]
  mimeUnrender _ bs = Right $ ([(1.0, 1, 1.0)] :: [(Double, Int, Double)]) -- Dummy value

data Version = Version {
  version :: Text
  } deriving (Show, Generic)

data Experiments = Experiments {
  experiments :: [Text]
  } deriving (Show, Generic)

instance FromJSON Experiments

data Scalar = Scalar {
  wallTime :: Double,
  step :: Int, -- Int required, double returns 500 Error
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
  :> ReqBody '[JSON] (Double, Int, Double)
  :> Post '[HTML] Text

  :<|> "data" :> "scalars"
  :> QueryParam "xp" Text
  :> QueryParam "name" Text
  --  :> Get '[HTML] [(Double, Int, Double)]
  :> Get '[HTML] Text -- TODO - replace with parsed value


type HistogramAPI =

  "data" :> "histograms"
  :> QueryParam "xp" Text :> QueryParam "name" Text :> QueryParam "toBuild" Bool
  :> ReqBody '[JSON] [Double] -- TODO : vary type depending on toBuild value
  :> Post '[HTML] Text

  :<|> "data" :> "histograms"
  :> QueryParam "xp" Text :> QueryParam "name" Text
  :> Get '[JSON] [[Double]] -- TODO - build type for histogram json

type API = ManagementAPI :<|> ScalarAPI :<|> HistogramAPI

clientVersion
  :<|> clientExperiments
  :<|> clientExperimentInfo
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

testListExperiments :: IO ()
testListExperiments = runTest clientExperiments

testListScalars :: Text -> IO ()
testListScalars expName = runTest $ clientExperimentInfo (Just expName)

testAddExperiment :: Text -> IO ()
testAddExperiment expName = runTest $ clientAddExperiment expName

testDeleteExperiment :: Text -> IO ()
testDeleteExperiment expName = runTest $ clientDeleteExperiment (Just expName)

testAddScalar :: Text -> Text -> Scalar -> IO ()
testAddScalar expName metricName scalar =
  runTest $ clientAddScalar (Just expName) (Just metricName) val
  where val = (wallTime scalar, step scalar, Main.value scalar)

testGetScalar :: Text -> Text -> IO ()
testGetScalar expName metricName =
  runTest $ clientGetScalar (Just expName) (Just metricName)

main :: IO ()
main = do
  putStrLn "Version"
  testVersion >> putStrLn ""

  putStrLn "List Experiments"
  testListExperiments >> putStrLn ""

  putStrLn "Add Experiment"
  testAddExperiment "test_experiment" >> putStrLn ""

  putStrLn "List Experiments (known crayon bug - doesn't show empty experiments)"
  testListExperiments >> putStrLn ""

  putStrLn "Experiment Info"
  testListScalars "test_experiment" >> putStrLn ""

  putStrLn "Add Scalar"
  testAddScalar "test_experiment" "foo" (Scalar (1.0) 1 2.0)
    >> putStrLn ""

  putStrLn "Get Scalar"
  testGetScalar "test_experiment" "foo" >> putStrLn ""

  putStrLn "List Experiments (again)"
  testListExperiments >> putStrLn ""

  putStrLn "Delete Experiment"
  testDeleteExperiment "test_experiment" >> putStrLn ""
