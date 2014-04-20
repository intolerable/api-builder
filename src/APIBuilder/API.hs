module APIBuilder.API
  ( API 
  , liftEither
  , liftBuilder
  , liftState
  , runAPI
  , runRoute
  , name
  , baseURL
  , customizeRoute
  , customizeRequest ) where

import APIBuilder.Builder
import APIBuilder.Decoding
import APIBuilder.Error
import APIBuilder.Routes

import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Conduit

type API s e a = EitherT (APIError e) (StateT Builder (StateT s IO)) a

liftEither :: API s e a -> API s e a
liftEither = id 

liftBuilder :: StateT Builder (StateT s IO) a -> API s e a
liftBuilder = lift

liftState :: StateT s IO a -> API s e a
liftState = lift . lift

runAPI :: Builder -> s -> API s e a -> IO (Either (APIError e) a)
runAPI b s api = evalStateT (evalStateT (runEitherT api) b) s

runRoute :: (FromJSON a, FromJSON e) => Route -> API s e a
runRoute route = do
  b <- liftBuilder get
  req <- hoistEither $ routeRequest b route `eitherOr` InvalidURLError
  resp <- liftIO $ withManager (httpLbs req)
  hoistEither $ decode $ responseBody resp

eitherOr :: Maybe a -> b -> Either b a
a `eitherOr` b =
  case a of 
    Just x -> Right x
    Nothing -> Left b

routeRequest :: Builder -> Route -> Maybe Request
routeRequest b route = 
  let initialURL = parseUrl (T.unpack $ routeURL (_baseURL b) (_customizeRoute b route)) in
  fmap (\url -> _customizeRequest b $ url { method = T.encodeUtf8 (httpMethod route) }) initialURL

name :: Text -> API s e ()
name t = liftBuilder $ modify (\b -> b { _name = t })

baseURL :: Text -> API s e ()
baseURL t = liftBuilder $ modify (\b -> b { _baseURL = t })

customizeRoute :: (Route -> Route) -> API s e ()
customizeRoute f = liftBuilder $ modify (\b -> b { _customizeRoute = f })

customizeRequest :: (Request -> Request) -> API s e ()
customizeRequest f = liftBuilder $ modify (\b -> b { _customizeRequest = f })
