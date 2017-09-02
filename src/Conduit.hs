{-# LANGUAGE OverloadedStrings #-}

module Conduit where

import Data.Conduit
import Servant.Client

import API
import Tests

testConduit = do
  testAddExperiment "test_experiment_conduit"
  testDeleteExperiment "test_experiment_conduit"
  pure ()
