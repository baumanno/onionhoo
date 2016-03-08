{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- | A summary document for relays
module Onionhoo.Summary.Relay (Relay) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

-- | Contains summary information for a relay
data Relay =
  Relay {n :: Maybe Text -- ^ nickname
        ,f :: Text -- ^ fingerprint
        ,a :: [Text] -- ^ list of IPv4/IPv6 addresses
        ,r :: Bool -- ^ running
        }
  deriving (Show, Generic)

instance FromJSON Relay
