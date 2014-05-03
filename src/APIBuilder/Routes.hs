module APIBuilder.Routes
  ( Route(..)
  , URLFragment
  , URLParam
  , (=.)
  , HTTPMethod(..)
  , showMethod
  , routeURL ) where

import Control.Arrow ((***))
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Base as HTTP (urlEncodeVars)

-- | Alias for @Text@ to store the URL fragments for each @Route@.
type URLFragment = Text

-- | Alias to @(Text, Maybe Text)@ used to store each query that gets
--   tacked onto the request.
type URLParam = (Text, Maybe Text)

-- | Convenience function for building @URLParam@s.
-- 
-- >>> "api_type" =. Just "json"
-- ("api_type", Just "json")
(=.) :: Text -> Maybe Text -> (Text, Maybe Text)
(=.) = (,)

-- | Main type for routes in the API. Used to represent the URL minus the actual 
--   endpoint URL as well as the query string and the HTTP method used to communicate
--   with the server.
data Route = Route { fragments :: [URLFragment]
                   , urlParams :: [URLParam]
                   , httpMethod :: HTTPMethod }
  deriving (Show, Read, Eq)

-- | Type for different HTTP request methods. Has the two most common ones (GET and POST)
--   as well as support for custom methods.
data HTTPMethod = GET
                | POST
                | CustomMethod Text
  deriving (Show, Read, Eq)

-- | Get a @String@ from a HTTPMethod. Used mostly for creating @Request@s.
--
-- >>> showMethod GET
-- "GET"
-- >>> showMethod (CustomMethod "PATCH")
-- "PATCH"
showMethod :: HTTPMethod -> String
showMethod GET = "GET"
showMethod POST = "POST"
showMethod (CustomMethod t) = T.unpack t

-- | Converts a Route to a URL. Drops any @Nothing@ values from the query, separates the
--   fragments with "/" and tacks them onto the end of the base URL.
routeURL :: Text -- ^ base URL for the @Route@ (you can usually get this from the @Builder@)
         -> Route -- ^ the @Route@ to process
         -> Text -- ^ the finalized URL as a @Text@
routeURL baseURL (Route fs ps _) =
  let path = T.intercalate "/" fs
  in baseURL <> "/" <> path <> pathParamsSep fs <> buildParams ps

pathParamsSep :: [URLFragment] -> Text
pathParamsSep [] = "?"
pathParamsSep xs = if T.isInfixOf "." (last xs) then "?" else "/?"

buildParams :: [URLParam] -> Text
buildParams = T.pack . HTTP.urlEncodeVars . map (T.unpack *** T.unpack) . mapMaybe collectParams

collectParams :: URLParam -> Maybe (Text, Text)
collectParams (a, Just x) = Just (a,x)
collectParams (_, Nothing) = Nothing
