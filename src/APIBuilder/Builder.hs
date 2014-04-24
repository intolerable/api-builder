module APIBuilder.Builder
  ( Builder(..)
  , basicBuilder ) where

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

basicBuilder :: Text -> Text -> Builder
basicBuilder n b = Builder n b id id 
