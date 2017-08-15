{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A Uptime document for relays
module Onionhoo.Uptime.Relay where

import Control.Monad (mzero)
import Data.Aeson
import GHC.Generics
import Onionhoo.History.Graph

-- | Contains Uptime information for a relay
data Relay = Relay
  { fingerprint :: String
  , uptime :: Maybe UptimeHistory
  , flags :: Maybe Object
  } deriving (Show, Generic)

data UptimeHistory = UptimeHistory
  { oneWeek :: Maybe Graph
  , oneMonth :: Maybe Graph
  , threeMonths :: Maybe Graph
  , oneYear :: Maybe Graph
  , fiveYears :: Maybe Graph
  } deriving (Show)

----
-- Instance declarations
----
instance FromJSON Relay

instance FromJSON UptimeHistory where
  parseJSON (Object v) =
    UptimeHistory <$> v .:? "1_week" <*> v .:? "1_month" <*> v .:? "3_months" <*>
    v .:? "1_year" <*>
    v .:? "5_years"
  parseJSON _ = mzero
