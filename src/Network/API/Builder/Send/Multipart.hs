module Network.API.Builder.Send.Multipart where

import Network.API.Builder.Builder
import Network.API.Builder.Routes
import Network.API.Builder.Send

import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Conduit (Request)

data Multipart = Multipart [Part]
  deriving (Show)

sendMultipart :: MonadIO m => Builder -> Route -> Multipart -> m (Maybe Request)
sendMultipart b r (Multipart ps) = do
  case send b r () of
    Nothing -> return Nothing
    Just req -> liftM Just $ formDataBody ps req
