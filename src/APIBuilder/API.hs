module APIBuilder.API (
  -- * API
    API
  , APIT
  -- ** Running the API
  , runAPI
  , runRoute
  , routeResponse
  , routeRequest
  -- ** Lifting
  , liftEither
  , liftBuilder
  , liftState
  -- ** Changing the @Builder@ within the API
  , name
  , baseURL
  , customizeRoute
  , customizeRequest ) where

import APIBuilder.Builder
import APIBuilder.Decoding
import APIBuilder.Error
import APIBuilder.Routes

import Control.Exception
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit

-- | Main API type. @s@ is the API's internal state, @e@ is the API's custom error type,
--   and @a@ is the result when the API runs. Based on the @APIT@ transformer.
type API s e a = APIT s e IO a

-- | Main API transformer type. @s@ is the API's internal state, @e@ is the API's custom error type,
--   and @a@ is the result when the API runs.
type APIT s e m a = EitherT (APIError e) (StateT Builder (StateT s m)) a

-- | Lifts an action that works on an @API@ to an action that works on an @API@.
--   This function is provided solely for future-proofing in the case that more transformers
--   need to be stacked on top - it's implemented simply as @id@ for the moment.
liftEither :: Monad m => EitherT (APIError e) (StateT Builder (StateT s m)) a -> APIT s e m a
liftEither = id

-- | Lifts an action that operates on a @Builder@ to one that works on an @API@. Useful
--   mainly for gaining access to a @Builder@ from inside an @API@.
liftBuilder :: Monad m => StateT Builder (StateT s m) a -> APIT s e m a
liftBuilder = lift

-- | Lifts an action on an @API@'s state type @s@ to one that works on the @API@. Good
--   for messing with the state from inside the @API@.
liftState :: Monad m => StateT s m a -> APIT s e m a
liftState = lift . lift

-- | Runs an @API@ by executing its transformer stack and dumping it all into @IO@.
runAPI :: MonadIO m
       => Builder -- ^ initial @Builder@ for the @API@
       -> s -- ^ initial state @s@ for the @API@
       -> APIT s e m a -- ^ the actual @API@ to run
       -> m (Either (APIError e) a) -- ^ IO action that returns either an error or the result
runAPI b s api = evalStateT (evalStateT (runEitherT api) b) s

-- | Runs a @Route@. Infers the type to convert to from the JSON with the @a@ in @API@,
--   and infers the error type from @e@.
runRoute :: (FromJSON a, FromJSON e, MonadIO m) => Route -> APIT s e m a
runRoute route = routeResponse route >>= hoistEither . decode . responseBody


-- | Runs a @Route@, but only returns the response and does nothing towards
--   decoding the response.
routeResponse :: (MonadIO m) => Route -> APIT s e m (Response ByteString)
routeResponse route = do
  b <- liftBuilder get
  req <- hoistEither $ routeRequest b route `eitherOr` InvalidURLError
  resp <- do
    r <- liftIO $ try $ withManager (httpLbs req)
    hoistEither $ either (Left . HTTPError) Right r
  return resp

eitherOr :: Maybe a -> b -> Either b a
a `eitherOr` b =
  case a of
    Just x -> Right x
    Nothing -> Left b

-- | Try to construct a @Request@ from a @Route@ (with the help of the @Builder@). Returns @Nothing@ if
--   the URL is invalid or there is another error with the @Route@.
routeRequest :: Builder -> Route -> Maybe Request
routeRequest b route =
  let initialURL = parseUrl (T.unpack $ routeURL (_baseURL b) (_customizeRoute b route)) in
  fmap (\url -> _customizeRequest b $ url { method = httpMethod route }) initialURL

-- | Modify the @name@ of the @Builder@ from inside an API. Using this is probably not the best idea,
--   it's nice if the @Builder@'s name is stable at least.
name :: Monad m => Text -> APIT s e m ()
name t = liftBuilder $ modify (\b -> b { _name = t })

-- | Modify the @baseURL@ of the @Builder@ from inside an API.
--   Can be useful for changing the API's endpoints for certain requests.
baseURL :: Monad m => Text -> APIT s e m ()
baseURL t = liftBuilder $ modify (\b -> b { _baseURL = t })

-- | Modify every @Route@ before it runs. Useful for adding extra params to every query,
--   for example.
customizeRoute :: Monad m => (Route -> Route) -> APIT s e m ()
customizeRoute f = liftBuilder $ modify (\b -> b { _customizeRoute = f })

-- | Modify every @Request@ before the API fetches it. Useful for adding headers to every request,
--   for example.
customizeRequest :: Monad m => (Request -> Request) -> APIT s e m ()
customizeRequest f = liftBuilder $ modify (\b -> b { _customizeRequest = f })
