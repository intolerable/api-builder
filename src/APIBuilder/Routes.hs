module APIBuilder.Routes
  ( URLFragment
  , URLParam
  , Route(..)
  , HTTPMethod(..)
  , showMethod
  , routeURL
  , (=.) ) where

import Control.Arrow ((***))
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Base as HTTP (urlEncodeVars)

type URLFragment = Text
type URLParam = (Text, Maybe Text)

(=.) :: Text -> Maybe Text -> (Text, Maybe Text)
(=.) = (,)

data Route = Route { fragments :: [URLFragment]
                   , urlParams :: [URLParam]
                   , httpMethod :: HTTPMethod }
  deriving (Show, Read, Eq)

data HTTPMethod = GET
                | POST
                | CustomMethod Text
  deriving (Show, Read, Eq)

showMethod :: HTTPMethod -> String
showMethod GET = "GET"
showMethod POST = "POST"
showMethod (CustomMethod t) = T.unpack t

routeURL :: Text -> Route -> Text
routeURL baseURL (Route fs ps _) =
  let path = T.intercalate "/" fs
  in baseURL <> "/" <> path <> pathParamsSep fs <> buildParams ps

pathParamsSep :: [URLFragment] -> Text
pathParamsSep [] = "?"
pathParamsSep xs = if T.isInfixOf "." (last xs) then "?" else "/?"

buildParams :: [URLParam] -> Text
buildParams = T.pack . HTTP.urlEncodeVars . map (T.unpack *** T.unpack) . mapMaybe collectParams

collectParams :: URLParam -> Maybe (Text, Text)
collectParams (a, Just x) = Just (a,x)
collectParams (_, Nothing) = Nothing
