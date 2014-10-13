module Network.API.Builder.Error
  ( APIError(..) ) where

import Network.HTTP.Conduit (HttpException)

-- | Error type for the @API@, where @a@ is the type that should be returned when
--   something goes wrong on the other end - i.e. any error that isn't directly related
--   to this library.
data APIError a = APIError a -- ^ A type that represents any error that happens on the API end.
                             --   Define your own custom type with a @FromJSON@ instance if you
                             --   want to handle them, or you can use @()@ if you just want to
                             --   ignore them all.
                | HTTPError HttpException -- ^ Something went wrong when we tried to do a HTTP operation.
                | InvalidURLError -- ^ You're trying to create an invalid URL somewhere - check your
                                  --   @Builder@'s base URL and your @Route@s.
                | ParseError String -- ^ Failed when parsing the response, and it wasn't an error on their end.
  deriving Show

instance Eq a => Eq (APIError a) where
  (HTTPError _) == _ = False
  _ == (HTTPError _) = False
  (APIError a) == (APIError b) = a == b
  InvalidURLError == InvalidURLError = True
  (ParseError a) == (ParseError b) = a == b
