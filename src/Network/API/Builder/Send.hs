{-# LANGUAGE CPP #-}
module Network.API.Builder.Send where

import Network.API.Builder.Builder
import Network.API.Builder.Routes

import Data.Aeson

#ifdef __GHCJS__

import JavaScript.Web.XMLHttpRequest
import           Data.JSString.Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
import Control.Arrow ((***))
import Data.String (IsString)
import Data.ByteString (ByteString)

#else

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

#endif


-- | Class for types that can be sent with api-builder.
--   Given a 'Builder', a 'Route', and an instance of 'Sendable', we should be able to construct a 'Request' for the API's server. If we can't, 'send' returns 'Nothing' and 'APIT' complains about being unable to send the given data.
class Sendable s where
  send :: Builder -> Route -> s -> Maybe Request

-- | By default, the '()' instance for 'Sendable' moves the query parameters of the 'Route' into the body of the POST request. Most APIs handle both, but some will complain if they aren't sent in the actual query. If you 'send' 'PostQuery' instead of '()', the query params won't move from the actual query string when constructing the request.
data PostQuery = PostQuery
  deriving (Show)

#ifdef __GHCJS__

instance Sendable () where
  send builder r () =
    let method = nameToMethod $ httpMethod r
        route = _customizeRoute builder r
        uri = routeURL (_baseURL builder) route
        req = Request{reqMethod = method
                     , reqWithCredentials = True
                     , reqURI = textToJSString uri
                     , reqLogin = Nothing
                     , reqHeaders = []
                     , reqData = if method == POST
                                 then paramsToForm $ urlParams route
                                 else NoData
                     }

    in
      Just $ _customizeRequest builder req
    where
      paramsToForm =
        FormData . concatMap (map (textToJSString *** (StringVal . textToJSString)))

instance Sendable Value where
  send builder r value =
    case nameToMethod $ httpMethod r of
      method@(POST) -> do
        let route = _customizeRoute builder r
            uri = routeURL (_baseURL builder) route
            req = Request{reqMethod = method
                         , reqWithCredentials = True
                         , reqURI = textToJSString uri
                         , reqLogin = Nothing
                         , reqHeaders = [("Content-Type", "application/json")]
                         , reqData = StringData . lazyTextToJSString . LTE.decodeUtf8 $
                                     encode value
                         }

          in
          Just $ _customizeRequest builder req
      _ ->
        Nothing

instance Sendable ByteString where
  send builder r bs =
    case httpMethod r of
      "POST" -> do
        req <- basicSend builder r
        return $ req {reqData = StringData . textToJSString . TE.decodeUtf8 $ bs}
      _ -> Nothing

instance Sendable PostQuery where
  send builder r PostQuery = basicSend builder r

basicSend :: Builder -> Route -> Maybe Request
basicSend builder r =
  let method = nameToMethod $ httpMethod r
      route = _customizeRoute builder r
      uri = routeURL (_baseURL builder) route
  in
    Just $ Request{reqMethod = method
                  , reqWithCredentials = True
                  , reqURI = textToJSString uri
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqData = NoData
                  }

nameToMethod :: (Eq a, Show a, IsString a) => a -> Method
nameToMethod "POST" = POST
nameToMethod "GET" = GET
nameToMethod "PUT" = PUT
nameToMethod "DELETE" = DELETE
nameToMethod m = error $ "unsupported http method " ++ show m

#else

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

useToJSON :: ToJSON a => Builder -> Route -> a -> Maybe Request
useToJSON b r v = send b r (toJSON v)

instance Sendable ByteString where
  send builder r bs =
    case httpMethod r of
      "POST" -> do
        req <- basicSend builder r
        return $ req { requestBody = RequestBodyLBS bs }
      _ -> Nothing

instance Sendable PostQuery where
  send builder r PostQuery = basicSend builder r

#endif
