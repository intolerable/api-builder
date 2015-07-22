module Network.API.Builder.Send where

import Network.API.Builder.Builder
import Network.API.Builder.Routes

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

-- | Class for types that can be sent with api-builder.
--   Given a 'Builder', a 'Route', and an instance of 'Sendable', we should be able to construct a 'Request' for the API's server. If we can't, 'send' returns 'Nothing' and 'APIT' complains about being unable to send the given data.
class Sendable s where
  send :: Builder -> Route -> s -> Maybe Request

instance Sendable () where
  send builder r () =
    case httpMethod r of
      "POST" -> do
        req <- parseUrl $ Text.unpack $ routeURL (_baseURL builder) (_customizeRoute builder r)
        return $ _customizeRequest builder $
          req { requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders req
              , requestBody = RequestBodyBS (dropQuestion $ queryString req)
              , queryString = ""
              , method = httpMethod r }
      _ -> basicSend builder r
    where dropQuestion b = if ByteString.isPrefixOf "?" b then ByteString.drop 1 b else b

basicSend :: Builder -> Route -> Maybe Request
basicSend builder r = do
  req <- parseUrl $ Text.unpack $ routeURL (_baseURL builder) (_customizeRoute builder r)
  return $ _customizeRequest builder $ req { method = httpMethod r }

instance Sendable Value where
  send builder r value =
    case httpMethod r of
      "POST" -> do
        req <- parseUrl $ Text.unpack $ routeURL (_baseURL builder) (_customizeRoute builder r)
        return $ _customizeRequest builder $
          req { requestBody = RequestBodyLBS (encode value)
              , requestHeaders = ("Content-Type", "application/json") : requestHeaders req
              , method = httpMethod r }
      _ -> Nothing

instance Sendable ByteString where
  send builder r bs =
    case httpMethod r of
      "POST" -> do
        req <- basicSend builder r
        return $ req { requestBody = RequestBodyLBS bs }
      _ -> Nothing

-- | By default, the '()' instance for 'Sendable' moves the query parameters of the 'Route' into the body of the POST request. Most APIs handle both, but some will complain if they aren't sent in the actual query. If you 'send' 'PostQuery' instead of '()', the query params won't move from the actual query string when constructing the request.
data PostQuery = PostQuery
  deriving (Show)

instance Sendable PostQuery where
  send builder r PostQuery = basicSend builder r
