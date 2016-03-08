{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Weights-document for bridges and relays
module Onionhoo.Weights (Weights) where

import qualified Onionhoo.Weights.Relay as R (Relay)

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)

-- | A Weights document containing information on bridges and relays
data Weights =
  Weights {version :: Text -- ^ the current API version
          ,nextMajorVersionScheduled :: Maybe Text -- ^ optional
          ,relaysPublished :: Text -- ^ UTC date when the list of relays was published
          ,relays :: [R.Relay] -- ^ list of relays
          ,bridgesPublished :: Text -- ^ UTC date when the list of bridges was published
          ,bridges :: Array} -- ^ list of bridges, empty for compatibility
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Weights)
