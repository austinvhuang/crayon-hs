{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tests where

import Data.Either (isRight)
import Data.Text
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Test.Hspec

import API

{- Convenience functioins -}

defaultEnv = do
  manager <- (newManager defaultManagerSettings)
  pure (ClientEnv manager (BaseUrl Http "localhost" 8889 ""))

handleResult res =
  case res of
    Left err -> (putStrLn $ "Error: " ++ show err) >> putStrLn "" >> (pure $ Left err)
    Right x -> print x >> putStrLn "" >> (pure $ Right x)

runTest clientFun = do
 res <- (defaultEnv >>= \env -> (runClientM clientFun) env)
 handleResult res

{- Tests of API functionality -}

testVersion = runTest clientVersion

testListExperiments = runTest clientExperiments

testListScalars expName = runTest $ clientExperimentInfo (Just expName)

testAddExperiment expName = runTest $ clientAddExperiment expName

testDeleteExperiment expName = runTest $ clientDeleteExperiment (Just expName)

testAddScalar expName metricName scalar =
  runTest $ clientAddScalar (Just expName) (Just metricName) val
  where val = (wallTime scalar, step scalar, API.value scalar)

testGetScalar expName metricName =
  runTest $ clientGetScalar (Just expName) (Just metricName)

testAddHistogram expName metricName histval =
  case hv of
    HistValues hvList -> runTest $ clientAddHistogram (Just expName) (Just metricName) (Just True) (ht, hs, hvList)
    _ -> runTest $ clientAddHistogram (Just expName) (Just metricName) (Just False) (ht, hs, []) -- TODO dummy value for now for HistSpec constructor
  where
    ht = htime histval
    hs = hstep histval
    hv = hvalue histval

testGetHistogram expName metricName =
  runTest $ clientGetHistogram (Just expName) (Just metricName)
