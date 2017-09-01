{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-} -- instances with lists
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BS
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Proxy
import GHC.Generics
import Control.Lens
import Control.Lens.Tuple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.Client
import Servant.API.ContentTypes (eitherDecodeLenient)

import API

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
  where val = (wallTime scalar, step scalar, API.value scalar)

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
