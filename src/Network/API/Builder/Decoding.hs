module Network.API.Builder.Decoding
  ( decode ) where

import Network.API.Builder.Error

import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Parser (value)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec.Lazy (parse, eitherResult)
import qualified Data.ByteString.Lazy as BS

-- | Try to parse a value from a JSON @ByteString@, and if not, try to
--   parse a useful error from the JSON. If we can't do that, complain about
--   a @ParseError@.
decode :: (FromJSON a, FromJSON e) => BS.ByteString -> Either (APIError e) a
decode s =
  case eitherDecode s of
    Right x -> Right x
    Left err ->
      case eitherDecode s of
        Right x -> Left $ APIError x
        Left _ -> Left $ ParseError err

-- | @Data.Aeson@'s @decode@ function will only parse a value if it's
--   an array or an object, in accordance with some RFC somewhere. Unfortunately
--   not all JSON APIs respect this, so we have to mess with parsers and values and
--   whatnot.
eitherDecode :: (FromJSON a) => BS.ByteString -> Either String a
eitherDecode s = eitherResult (parse value s) >>= parseEither parseJSON
