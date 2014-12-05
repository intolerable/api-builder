module Network.API.Builder.Send where

import Network.API.Builder.Builder
import Network.API.Builder.Routes

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit
import qualified Data.Text as Text

class Sendable s where
  send :: Builder -> Route -> s -> Maybe Request

instance Sendable () where
  send builder r () = do
    req <- parseUrl $ Text.unpack $ routeURL (_baseURL builder) (_customizeRoute builder r)
    return $ _customizeRequest builder $ req { method = httpMethod r }

instance Sendable Value where
  send builder r value =
    case httpMethod r of
      "POST" -> do
        req <- send builder r ()
        return $ req { requestBody = RequestBodyLBS (encode value) }
      _ -> Nothing

instance Sendable ByteString where
  send builder r bs =
    case httpMethod r of
      "POST" -> do
        req <- send builder r ()
        return $ req { requestBody = RequestBodyLBS bs }
      _ -> Nothing
