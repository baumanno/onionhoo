{-# LANGUAGE DeriveGeneric #-}

-- | A summary document for bridges
module Onionhoo.Summary.Bridge where

import Data.Aeson
import GHC.Generics

-- | Contains summary information for a bridge
data Bridge = Bridge
  { n :: Maybe String -- ^ nickname
  , h :: String -- ^ fingerprint, SHA1
  , r :: Bool -- ^ running
  } deriving (Show, Generic)

instance FromJSON Bridge
