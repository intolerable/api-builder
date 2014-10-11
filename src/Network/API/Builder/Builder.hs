module Network.API.Builder.Builder
  ( Builder(..)
  , basicBuilder ) where

import Network.API.Builder.Routes

import Data.Text (Text)
import Network.HTTP.Conduit (Request)
import qualified Data.Text as T

-- | Builder type for the API. Keeps track of the API's name and base URL, and how
--   to modify Routes and Requests before they're run.
data Builder = Builder { _name :: Text
                       , _baseURL :: Text
                       , _customizeRoute :: Route -> Route
                       , _customizeRequest :: Request -> Request }

instance Show Builder where
  show b = "Builder { name = " ++ T.unpack (_name b) ++ "}"

-- | Makes a basic builder, i.e. one that simply has a name and base URL
--   and doesn't fiddle with Routes / Requests.
basicBuilder :: Text -- ^ name
             -> Text -- ^ base url
             -> Builder -- ^ a simple @Builder@
basicBuilder n b = Builder n b id id
