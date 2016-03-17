{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A Uptime document for bridges
module Onionhoo.Uptime.Bridge where

import Onionhoo.History.Graph
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

-- | Contains Uptime information for a bridge
data Bridge =
  Bridge {fingerprint :: String
         ,uptime :: Maybe UptimeHistory}
  deriving (Show, Generic)

instance FromJSON Bridge

data UptimeHistory =
  UptimeHistory {oneWeek :: Maybe Graph
                ,oneMonth :: Maybe Graph
                ,threeMonths :: Maybe Graph
                ,oneYear :: Maybe Graph
                ,fiveYears :: Maybe Graph}
  deriving (Show)

instance FromJSON UptimeHistory where
  parseJSON (Object v) = 
    UptimeHistory <$>
    v .:? "1_week" <*>
    v .:? "1_month" <*>
    v .:? "3_months" <*>
    v .:? "1_year" <*>
    v .:? "5_years"
  parseJSON _ = mzero
