{-# LANGUAGE DeriveGeneric #-}

-- | A summary document for relays
module Onionhoo.Summary.Relay where

import Data.Aeson
import GHC.Generics

-- | Contains summary information for a relay
data Relay = Relay
  { n :: Maybe String -- ^ nickname
  , f :: String -- ^ fingerprint
  , a :: [String] -- ^ list of IPv4/IPv6 addresses
  , r :: Bool -- ^ running
  } deriving (Show, Generic)

instance FromJSON Relay
