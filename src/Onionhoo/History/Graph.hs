{-# LANGUAGE DeriveGeneric #-}

-- |A graph-history document
module Onionhoo.History.Graph where

import Data.Aeson
import GHC.Generics

data Graph = Graph
  { first :: String
  , last :: String
  , interval :: Int
  , factor :: Double
  , count :: Maybe Int
  , values :: [Maybe Int]
  } deriving (Show, Generic)

instance FromJSON Graph
