module Network.API.Builder.Send.Multipart
  ( sendMultipart
  , Multipart(..) ) where

import Network.API.Builder.Builder
import Network.API.Builder.Routes
import Network.API.Builder.Send

import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client (Request)

-- | A type for multipart forms, which uses 'Part's from 'Network.HTTP.Client.MultipartFormData'.
--   Construct it and send it with 'sendMultipart' (not 'send').
data Multipart = Multipart [Part]
  deriving (Show)

-- | Send a 'Multipart' request. This can't use the normal 'send' mechanism since
--   it has to do IO to construct its request.
sendMultipart :: MonadIO m => Builder -> Route -> Multipart -> m (Maybe Request)
sendMultipart b r (Multipart ps) = do
  case send b r () of
    Nothing -> return Nothing
    Just req -> liftM Just $ formDataBody ps req
