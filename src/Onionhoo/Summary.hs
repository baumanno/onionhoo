{-# LANGUAGE OverloadedStrings #-}

-- | A summary-document for bridges and relays
module Onionhoo.Summary (Summary) where

import qualified Onionhoo.Summary.Bridge as B (Bridge)
import qualified Onionhoo.Summary.Relay as R (Relay)

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)

-- | A summary document containing information on bridges and relays
data Summary =
  Summary {version :: Text -- ^ the current API version
          ,nextMajorVersionScheduled :: Maybe Text -- ^ optional
          ,relaysPublished :: Text -- ^ UTC date when the list of relays was published
          ,relays :: [R.Relay] -- ^ list of relays
          ,bridgesPublished :: Text -- ^ UTC date when the list of bridges was published
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

