module Network.API.Builder.Routes
  ( Route(..)
  , URLPiece
  , URLParam
  , (=.)
  , routeURL ) where

import Control.Arrow ((***))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Base as HTTP (urlEncodeVars)
import qualified Network.HTTP.Types.Method as HTTP

import Network.API.Builder.Query

-- | Alias for @Text@ to store the URL fragments for each @Route@.
type URLPiece = Text

-- | Alias to @(Text, Maybe Text)@ used to store each query that gets
--   tacked onto the request.
type URLParam = [(Text, Text)]

-- | Convenience function for building @URLParam@s. Right-hand argument must
--   have a @ToQuery@ instance so it can be converted to the appropriate
--   representation in a query string. Query values do not need to be
--   escaped.
--
-- >>> "api_type" =. ("json" :: Text)
-- ("api_type", Just "json")
(=.) :: ToQuery a => Text -> a -> [(Text, Text)]
k =. v = toQuery k v

-- | Main type for routes in the API. Used to represent the URL minus the actual
--   endpoint URL as well as the query string and the HTTP method used to communicate
--   with the server.
data Route = Route { urlPieces :: [URLPiece]
                   , urlParams :: [URLParam]
                   , httpMethod :: HTTP.Method }
  deriving (Show, Read, Eq)

-- | Converts a Route to a URL. Drops any @Nothing@ values from the query, separates the
--   fragments with "/" and tacks them onto the end of the base URL.
routeURL :: Text -- ^ base URL for the @Route@ (you can usually get this from the @Builder@)
         -> Route -- ^ the @Route@ to process
         -> Text -- ^ the finalized URL as a @Text@
routeURL baseURL (Route fs ps _) =
  baseURL <> firstSep <> path <> querySep <> buildParams ps
  where
    firstSep = if null fs then T.empty else "/"
    path = T.intercalate "/" fs
    querySep = if null ps then T.empty else "" fs

buildParams :: [URLParam] -> Text
buildParams = T.pack . HTTP.urlEncodeVars . concatMap (map (T.unpack *** T.unpack))
