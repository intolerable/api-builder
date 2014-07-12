module APIBuilder.Query where

import Data.Text (Text)
import qualified Data.Text as Text

class ToQuery a where
  toQuery :: a -> Maybe Text

instance ToQuery String where
  toQuery = Just . Text.pack

instance ToQuery Text where
  toQuery = Just

instance ToQuery a => ToQuery (Maybe a) where
  toQuery (Just a) = toQuery a
  toQuery Nothing = Nothing
