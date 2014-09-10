module Network.API.Builder.Query where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

class ToQuery a where
  toQuery :: a -> Maybe Text

instance ToQuery Integer where
  toQuery = Just . Text.pack . show

instance ToQuery Bool where
  toQuery True = Just "true"
  toQuery False = Just "false"

instance ToQuery Int where
  toQuery = Just . Text.pack . show

instance ToQuery Text where
  toQuery = Just

instance ToQuery a => ToQuery (Maybe a) where
  toQuery (Just a) = toQuery a
  toQuery Nothing = Nothing

instance ToQuery a => ToQuery [a] where
  toQuery [] = Nothing
  toQuery xs = Just $ Text.intercalate "," $ mapMaybe toQuery xs
