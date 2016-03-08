{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- | A summary document for bridges
module Onionhoo.Summary.Bridge (Bridge) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

-- | Contains summary information for a bridge
data Bridge =
  Bridge {n :: Maybe Text -- ^ nickname
         ,h :: Text -- ^ fingerprint, SHA1
         ,r :: Bool -- ^ running
         }
  deriving (Show, Generic)

instance FromJSON Bridge
