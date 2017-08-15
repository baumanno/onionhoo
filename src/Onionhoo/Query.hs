{-# LANGUAGE OverloadedStrings #-}

-- | A query-manager for interfacing with the Onionoo-API
module Onionhoo.Query
  ( summaryQuery
  , detailQuery
  , bandwidthQuery
  , clientsQuery
  , weightsQuery
  , uptimeQuery
  , module Onionhoo.Summary
  ) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.List (intercalate)
import Network.HTTP.Conduit (simpleHttp)

-- import the different document-types
import Onionhoo.Bandwidth
import Onionhoo.Clients
import Onionhoo.Detail
import Onionhoo.Summary
import Onionhoo.Uptime
import Onionhoo.Weights

-- | the API endpoint for Onionoo
apiUrl :: String
apiUrl = "https://onionoo.torproject.org"

-- | Generic helper for issuing a request to the API
performRequest ::
     (FromJSON a)
  => String -- ^ the document to request, e.g. 'summary'
  -> IO (Either String a) -- ^ on success, the parsed document
performRequest url = eitherDecode <$> simpleHttp url

summaryQuery :: [String] -> IO (Either String Summary)
summaryQuery params = performRequest $ buildURL "summary" params

detailQuery :: [String] -> IO (Either String Detail)
detailQuery params = performRequest $ buildURL "details" params

bandwidthQuery :: [String] -> IO (Either String Bandwidth)
bandwidthQuery params = performRequest $ buildURL "bandwidth" params

weightsQuery :: [String] -> IO (Either String Weights)
weightsQuery params = performRequest $ buildURL "weights" params

clientsQuery :: [String] -> IO (Either String Clients)
clientsQuery params = performRequest $ buildURL "clients" params

uptimeQuery :: [String] -> IO (Either String Uptime)
uptimeQuery params = performRequest $ buildURL "uptime" params

buildURL :: String -> [String] -> String
buildURL doc params = concat [apiUrl, "/", doc, mkParams params]
  where
    mkParams :: [String] -> String
    mkParams [] = "" -- this will effectively fetch the whole consensus!
    mkParams xs = "?" ++ intercalate "&" xs
