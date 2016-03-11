{-# LANGUAGE OverloadedStrings #-}

-- | A query-manager for interfacing with the Onionoo-API
module Onionhoo.Query
       (summaryQuery
       , detailQuery
       , bandwidthQuery
       , clientsQuery
       , weightsQuery,
       uptimeQuery)
       where

-- import the different document-types
import Onionhoo.Bandwidth
import Onionhoo.Clients
import Onionhoo.Detail
import Onionhoo.Summary
import Onionhoo.Uptime
import Onionhoo.Weights
import Control.Applicative ((<$>))
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Maybe

apiUrl :: String
apiUrl = "http://onionoo.torproject.org"

performRequest :: (FromJSON a)
               => String -> IO (Either String a)
performRequest url = eitherDecode <$> simpleHttp url

summaryQuery
  :: [String] -> IO (Either String Summary)
summaryQuery params = performRequest $ buildURL "summary" params

detailQuery
  :: [String] -> IO (Either String Detail)
detailQuery params = performRequest $ buildURL "details" params

bandwidthQuery
  :: [String] -> IO (Either String Bandwidth)
bandwidthQuery params = performRequest $ buildURL "bandwidth" params

weightsQuery
  :: [String] -> IO (Either String Weights)
weightsQuery params = performRequest $ buildURL "weights" params

clientsQuery
  :: [String] -> IO (Either String Clients)
clientsQuery params = performRequest $ buildURL "clients" params

uptimeQuery
  :: [String] -> IO (Either String Uptime)
uptimeQuery params = performRequest $ buildURL "uptime" params

buildURL :: String -> [String] -> String
buildURL doc params = concat [apiUrl,"/",doc,mkParams params]
  where mkParams :: [String] -> String
        mkParams [] = "" -- ^ this will effectively fetch the whole consensus!
        mkParams xs = "?" ++ intercalate "&" xs
