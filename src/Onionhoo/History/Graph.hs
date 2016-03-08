{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |A graph-history document
module Onionhoo.History.Graph where

import Data.Aeson
import Data.Text
import GHC.Generics

data Graph =
  Graph {first :: Text
        ,last :: Text
        ,interval :: Int
        ,factor :: Double
        ,count :: Maybe Int
        ,values :: [Maybe Int]}
  deriving (Show,Generic)

instance FromJSON Graph
