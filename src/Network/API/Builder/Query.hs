module Network.API.Builder.Query where

import Data.Text (Text)
import qualified Data.Text as Text

class ToQuery a where
  toQuery :: Text -> a -> [(Text, Text)]

instance ToQuery Integer where
  toQuery k v = [(k, Text.pack $ show v)]

instance ToQuery Bool where
  toQuery k True = [(k, "true")]
  toQuery k False = [(k, "false")]

instance ToQuery Int where
  toQuery k v = [(k, Text.pack $ show v)]

instance ToQuery Text where
  toQuery k v = [(k, v)]

instance ToQuery a => ToQuery (Maybe a) where
  toQuery k (Just a) = toQuery k a
  toQuery _ Nothing = []

instance ToQuery a => ToQuery [a] where
  toQuery _ [] = []
  toQuery k xs = [(k, Text.intercalate "," $ map snd $ toQuery k xs)]

newtype ArrayList a = ArrayList [a]

instance ToQuery a => ToQuery (ArrayList a) where
  toQuery _ (ArrayList []) = []
  toQuery k (ArrayList xs) = for (zip [1..] xs) $ \(n, x) ->
    (k <> "[" <> tshow n <> "]", x)
