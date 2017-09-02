{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

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

-- main :: IO ()
main = do
  hspec $ do
    describe "Crayon" $ do
      it "returns crayon version" $ do
        v <- testVersion
        v `shouldBe` Right Version {version = ("0.5" :: Text)}

  -- experiments
      it "initially returns an empty list of experiments" $ do
        lst <- testListExperiments
        lst `shouldBe` Right []
      it "allows adding test_experiment1" $ do
        res <- testAddExperiment "test_experiment1"
        res `shouldBe` Right "ok"
      it "allows adding test_experiment2" $ do
        res <- testAddExperiment "test_experiment2"
        res `shouldBe` Right "ok"
      it "lists no experiments prior to adding data (known crayon bug)" $ do
        lst <- testListExperiments
        lst `shouldBe` Right []

  -- scalars
      it "lists scalars for experiment 1 (TODO parse this properly)" $ do
        lst <- testListScalars "test_experiment1"
        lst `shouldBe` Right "{\"histograms\": [], \"scalars\": []}"
      it "allows adding a scalar value" $ do
        res <- testAddScalar "test_experiment1" "foo" (Scalar 1.0 1 2.0)
        res `shouldBe` Right "ok"
      it "allows adding another scalar value" $ do
        res <- testAddScalar "test_experiment1" "foo" (Scalar 2.5 2 4.0)
        res `shouldBe` Right "ok"
      it "allows adding another scalar value" $ do
        res <- testAddScalar "test_experiment1" "foo" (Scalar 2.6 3 1.0)
        res `shouldBe` Right "ok"
      it "allows getting a scalar varibale" $ do
        res <- testGetScalar "test_experiment1" "foo"
        res `shouldBe` Right [(1.0,1,2.0),(2.5,2,4.0),(2.6,3,1.0)]

  -- histogramss
      it "allows adding a histogram" $ do
        res <- testAddHistogram "test_experiment1" "hfoo"
          (Histogram 1.0 1 (HistValues [1.0, 1.0, 2.0, 3.0, 3.0, 3.0]))
        res `shouldBe` Right "ok"
      it "allows adding another histogram" $ do
        res <- testAddHistogram "test_experiment1" "hfoo2"
          (Histogram 1.0 1 (HistValues [3.0]))
        res `shouldBe` Right "ok"
      it "allows getting a histogram" $ do
        res <- testGetHistogram "test_experiment1" "hfoo"
        print res
        isRight res `shouldBe` True
      it "allows getting another histogram" $ do
        res <- testGetHistogram "test_experiment1" "hfoo2"
        print res
        isRight res `shouldBe` True

  -- quirks of experiment listings
      it "shows experiment with data in it (1/2 experimernts)" $ do
        res <- testListExperiments
        res `shouldBe` Right ["test_experiment1"]
      it "can add scalar to experiment 2" $ do
        res <- testAddScalar "test_experiment2" "foobar" (Scalar 0.0 1 1.0)
        res `shouldBe` Right "ok"
      it "lists shows 2/2 experiments" $ do
        res <- testListExperiments
        res `shouldBe` Right ["test_experiment2","test_experiment1"]

  -- delet experiments
      it "allows deleting an experiment" $ do
        res <- testDeleteExperiment "test_experiment1"
        res `shouldBe` Right "ok"
      it "allows deleting another experiment" $ do
        res <- testDeleteExperiment "test_experiment2"
        res `shouldBe` Right "ok"
