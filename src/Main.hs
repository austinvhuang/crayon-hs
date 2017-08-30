{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-} -- instances with lists

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
import Servant.API.ContentTypes (eitherDecodeLenient)

{- Types and Decoding/Encoding -}

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML Text where
  mimeRender _ = BSC.pack . T.unpack

instance MimeUnrender HTML Text where
  mimeUnrender _ bs = Right . pack . BSC.unpack $ bs

instance MimeUnrender HTML Version where
  mimeUnrender _ bs = Right . Version . pack . BSC.unpack $ bs

instance MimeUnrender HTML [Text] where
  mimeUnrender _ bs = (eitherDecodeLenient bs) :: Either String [Text]

instance MimeUnrender HTML [(Double, Int, Double)] where
  mimeUnrender _ bs =
    (eitherDecodeLenient bs) :: Either String [(Double, Int, Double)]

instance MimeUnrender HTML [[(Double, Int, [Double])]] where
  mimeUnrender _ bs =
    (eitherDecodeLenient bs) :: Either String [[(Double, Int, [Double])]]

-- Raw tuple representation of histogram data
type HistTuple = [(Double, Int, (Double, Double, Double, Double, Double,
                                 [Double], [Double]))]
instance MimeUnrender HTML HistTuple where
  mimeUnrender _ bs =
    (eitherDecodeLenient bs) :: Either String HistTuple

data Version = Version {
  version :: Text
  } deriving (Show, Generic)

data Scalar = Scalar {
  wallTime :: Double,
  step :: Int, -- Int required, double returns 500 Error
  value :: Double
  } deriving (Show, Generic)

instance FromJSON Scalar
instance ToJSON Scalar

data HistValues = HistValues [Double]
                | HistSpec {
                    hmin :: Double,
                    hmax :: Double,
                    hnum :: Int,
                    hbucketLimit :: [Double],
                    hbucket :: [Double],
                    hsum :: Maybe Double,
                    hsumSquares :: Maybe Double
                    } deriving (Show, Generic)

instance FromJSON HistValues
instance ToJSON HistValues

data Histogram =
  Histogram {
  htime :: Double,
  hstep :: Int,
  hvalue :: HistValues
  } deriving (Show, Generic)

instance FromJSON Histogram
-- TODO - derive this manually according to list formatting
-- derived from:
-- https://github.com/torrvision/crayon/blob/master/doc/specs.md#histogram-data
instance ToJSON Histogram

{- Specify Crayon API and Generate Client Functions -}

type ManagementAPI =
  Get '[HTML] Version
  :<|> "data" :> Get '[HTML] [Text]
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
  :> Get '[HTML] [(Double, Int, Double)]

type HistogramAPI =

  "data" :> "histograms"
  :> QueryParam "xp" Text :> QueryParam "name" Text :> QueryParam "tobuild" Bool
  :> ReqBody '[JSON] (Double, Int, [Double])
  -- TODO - allow HistSpec to be passed
  :> Post '[HTML] Text

  :<|> "data" :> "histograms"
  :> QueryParam "xp" Text :> QueryParam "name" Text
  :> Get '[HTML] HistTuple -- TODO: replace with parse into ADT

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
    Left err -> (putStrLn $ "Error: " ++ show err) >> putStrLn ""
    Right x -> print x >> putStrLn ""

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

testAddHistogram :: Text -> Text -> Histogram -> IO ()
testAddHistogram expName metricName histval =
  case hv of
    HistValues hvList ->
      runTest $ clientAddHistogram (Just expName) (Just metricName) (Just True) (ht, hs, hvList)
    _ -> -- TODO dummy value for now for HistSpec constructor
      runTest $ clientAddHistogram (Just expName) (Just metricName) (Just False) (ht, hs, [])
  where
    ht = htime histval
    hs = hstep histval
    hv = hvalue histval

testGetHistogram :: Text -> Text -> IO ()
testGetHistogram expName metricName =
  runTest $ clientGetHistogram (Just expName) (Just metricName)

main :: IO ()
main = do
  putStrLn "Version"
  testVersion

  putStrLn "List Experiments"
  testListExperiments

  putStrLn "Add Experiment"
  testAddExperiment "test_experiment1"

  putStrLn "Add Experiment"
  testAddExperiment "test_experiment2"

  putStrLn "List Experiments (known crayon bug - doesn't show empty experiments)"
  testListExperiments

  putStrLn "Experiment Info"
  testListScalars "test_experiment1"

  putStrLn "Add Scalar"
  testAddScalar "test_experiment1" "foo" (Scalar 1.0 1 2.0)

  putStrLn "Add Scalar"
  testAddScalar "test_experiment1" "foo" (Scalar 2.5 2 4.0)

  putStrLn "Add Scalar"
  testAddScalar "test_experiment1" "foo" (Scalar 2.6 3 1.0)

  putStrLn "Get Scalar"
  testGetScalar "test_experiment1" "foo"

  putStrLn "Add Histogram"
  testAddHistogram "test_experiment1" "hfoo"
    (Histogram 1.0 1 (HistValues [1.0, 1.0, 2.0, 3.0, 3.0, 3.0]))

  putStrLn "Add Histogram"
  testAddHistogram "test_experiment1" "hfoo"
    (Histogram 1.0 1 (HistValues [1.0, 1.0, 3.0, 3.0, 3.0, 3.0]))

  putStrLn "Add Histogram"
  testAddHistogram "test_experiment1" "hfoo2"
    (Histogram 1.0 1 (HistValues [3.0]))

  putStrLn "Get Histogram"
  testGetHistogram "test_experiment1" "hfoo"

  putStrLn "Get Histogram"
  testGetHistogram "test_experiment1" "hfoo2"

  putStrLn "List Experiments (shows 1/2 experiments)"
  testListExperiments

  putStrLn "Add Scalar"
  testAddScalar "test_experiment2" "foobar" (Scalar 0.0 1 1.0)

  putStrLn "List Experiments (shows 2/2 experiments)"
  testListExperiments

  putStrLn "Delete Experiment"
  testDeleteExperiment "test_experiment1"

  putStrLn "Delete Experiment"
  testDeleteExperiment "test_experiment2"
