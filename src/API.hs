{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-} -- instances with lists
{-# LANGUAGE FlexibleContexts #-}

module API where

import Control.Lens ((^.))
import Control.Lens.Tuple
import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BS
import Data.Text
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Media ((//), (/:))
import Data.Proxy
import Servant.API
import Servant.Client
import Servant.API.ContentTypes (eitherDecodeLenient)

{- Types and Decoding/Encoding -}

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML Text where
  mimeRender _ = BSC.pack . unpack

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

type HistTuple = (Double, Int, (Double, Double, Int, Double, Double,
                                 [Double], [Double]))
instance MimeUnrender HTML [HistTuple] where
  mimeUnrender _ bs =
    (eitherDecodeLenient bs) :: Either String [HistTuple]

instance MimeUnrender HTML [Histogram] where
  mimeUnrender _ bs = (fmap tuple2hist) <$> parse
    where parse = (eitherDecodeLenient bs) :: Either String [HistTuple]

tuple2hist :: HistTuple -> Histogram
tuple2hist tup = Histogram {
  htime = tup ^. _1,
  hstep = tup ^. _2,
  hvalue = HistSpec {
      hmin = tup ^. _3 ^. _1,
      hmax = tup ^. _3 ^. _2,
      hnum = tup ^. _3 ^. _3,
      hsum = Just $ tup ^. _3 ^. _4,
      hsumSquares = Just $ tup ^. _3 ^. _5,
      hbucketLimit = tup ^. _3 ^. _6,
      hbucket = tup ^. _3 ^. _7
      }
  }

data Version = Version {
  version :: Text
  } deriving (Eq, Generic, Show)

data Scalar = Scalar {
  wallTime :: Double,
  step :: Int,
  value :: Double
  } deriving (Eq, Generic, Show)

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
                    } deriving (Eq, Generic, Show)

instance FromJSON HistValues
instance ToJSON HistValues

data Histogram =
  Histogram {
  htime :: Double,
  hstep :: Int,
  hvalue :: HistValues
  } deriving (Eq, Generic, Show)

instance FromJSON Histogram
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
  :> Get '[HTML] [Histogram]

type API = ManagementAPI :<|> ScalarAPI :<|> HistogramAPI

clientVersion
  :<|> clientExperiments
  :<|> clientExperimentInfo
  :<|> clientAddExperiment
  :<|> clientDeleteExperiment = client (Proxy :: Proxy ManagementAPI)

clientAddScalar :<|> clientGetScalar = client (Proxy :: Proxy ScalarAPI)

clientAddHistogram :<|> clientGetHistogram = client (Proxy :: Proxy HistogramAPI)
