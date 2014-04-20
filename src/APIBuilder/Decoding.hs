module APIBuilder.Decoding
  ( decode ) where

import APIBuilder.Error

import Data.Aeson.Types (parseEither)
import Data.Aeson.Parser (value)
import Data.Aeson (FromJSON, parseJSON)
import Data.Attoparsec.Lazy (parse, eitherResult)
import qualified Data.ByteString.Lazy as BS

decode :: (FromJSON a, FromJSON e) => BS.ByteString -> Either (APIError e) a
decode s = 
  case eitherDecode s of
    Right x -> Right x
    Left err ->
      case eitherDecode s of 
        Right x -> Left $ APIError x
        Left _ -> Left $ ParseError err

eitherDecode :: (FromJSON a) => BS.ByteString -> Either String a
eitherDecode s = eitherResult (parse value s) >>= parseEither parseJSON
