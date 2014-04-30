module APIBuilder.Error
  ( APIError(..) ) where

import Network.HTTP.Conduit (HttpException)

data APIError a = APIError a
                | HTTPError HttpException
                | InvalidURLError
                | ParseError String
  deriving Show
