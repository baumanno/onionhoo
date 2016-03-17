{-# LANGUAGE OverloadedStrings #-}

-- | A summary-document for bridges and relays
module Onionhoo.Summary where

import qualified Onionhoo.Summary.Bridge as B (Bridge)
import qualified Onionhoo.Summary.Relay as R (Relay)

import Control.Monad (mzero)
import Data.Aeson

-- | A summary document containing information on bridges and relays
data Summary =
  Summary {version :: String -- ^ the current API version
          ,nextMajorVersionScheduled :: Maybe String -- ^ optional
          ,relaysPublished :: String -- ^ UTC date when the list of relays was published
          ,relays :: [R.Relay] -- ^ list of relays
          ,bridgesPublished :: String -- ^ UTC date when the list of bridges was published
          ,bridges :: [B.Bridge]} -- ^ list of bridges
  deriving (Show)

instance FromJSON Summary where
  parseJSON (Object v) = 
    Summary <$> v .: "version" <*>
    v .:? "next_major_version_scheduled" <*>
    v .: "relays_published" <*>
    v .: "relays" <*>
    v .: "bridges_published" <*>
    v .: "bridges"
  parseJSON _ = mzero

