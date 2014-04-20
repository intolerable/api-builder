module APIBuilder.Error
  ( APIError(..) ) where

data APIError a = APIError a
                | InvalidURLError
                | ParseError String
  deriving Show
