module APIBuilder.Builder
  ( Builder(..) ) where

import APIBuilder.Routes

import Network.HTTP.Conduit (Request)
import Data.Text (Text)
import qualified Data.Text as T

data Builder = Builder { _name :: Text
                       , _baseURL :: Text
                       , _customizeRoute :: Route -> Route
                       , _customizeRequest :: Request -> Request }

instance Show Builder where
  show b = "Builder { name = " ++ T.unpack (_name b) ++ "}"
