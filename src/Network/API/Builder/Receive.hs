module Network.API.Builder.Receive where

import Network.API.Builder.Error

import Control.Applicative
import Data.Aeson hiding (decode)
import Data.Aeson.Parser
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client

class Receivable r where
  receive :: ErrorReceivable e => Response ByteString -> Either (APIError e) r

instance Receivable ByteString where
  receive = Right . responseBody

instance Receivable (Response ByteString) where
  receive = Right

instance Receivable Value where
  receive = useFromJSON

instance (Receivable a, Receivable b) => Receivable (a, b) where
  receive x = (,) <$> receive x <*> receive x

instance (Receivable a, Receivable b, Receivable c) => Receivable (a, b, c) where
  receive x = (,,) <$> receive x <*> receive x <*> receive x

instance (Receivable a, Receivable b, Receivable c, Receivable d) => Receivable (a, b, c, d) where
  receive x = (,,,) <$> receive x <*> receive x <*> receive x <*> receive x

instance (Receivable a, Receivable b, Receivable c, Receivable d, Receivable e) => Receivable (a, b, c, d, e) where
  receive x = (,,,,) <$> receive x <*> receive x <*> receive x <*> receive x <*> receive x

useFromJSON :: (FromJSON a, ErrorReceivable e) => Response ByteString -> Either (APIError e) a
useFromJSON resp =
  case eitherDecode $ responseBody resp of
    Left err ->
      case receiveError resp of
        Just x -> Left $ APIError x
        Nothing -> Left $ ParseError err
    Right x -> return x

class ErrorReceivable e where
  receiveError :: Response ByteString -> Maybe e

instance ErrorReceivable ByteString where
  receiveError = Just . responseBody

instance ErrorReceivable () where
  receiveError _ = Nothing

instance ErrorReceivable Value where
  receiveError = useErrorFromJSON

useErrorFromJSON :: FromJSON a => Response ByteString -> Maybe a
useErrorFromJSON resp =
  case eitherDecode (responseBody resp) of
    Right x -> Just x
    Left _ -> Nothing

newtype JSONResponse a = JSONResponse { unwrapJSON :: a }
  deriving (Show, Read, Eq, Ord)

instance FromJSON a => FromJSON (JSONResponse a) where
  parseJSON v = JSONResponse `fmap` parseJSON v

instance FromJSON a => Receivable (JSONResponse a) where
  receive = useFromJSON

