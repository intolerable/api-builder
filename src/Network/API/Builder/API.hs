module Network.API.Builder.API (
  -- * API
    API
  , APIT
  -- ** Running the API
  , execAPI
  , runAPI
  , runRoute
  , sendRoute
  , routeResponse
  , routeRequest
  -- ** Lifting
  , liftEither
  , liftManager
  , liftBuilder
  , liftState
  -- ** Changing the @Builder@ within the API
  , name
  , baseURL
  , customizeRoute
  , customizeRequest ) where

import Network.API.Builder.Builder
import Network.API.Builder.Error
import Network.API.Builder.Receive
import Network.API.Builder.Routes
import Network.API.Builder.Send

import Data.Bifunctor
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Conduit

-- | Main API type. @s@ is the API's internal state, @e@ is the API's custom error type,
--   and @a@ is the result when the API runs. Based on the @APIT@ transformer.
type API s e a = APIT s e IO a

-- | Main API transformer type. @s@ is the API's internal state, @e@ is the API's custom error type,
--   and @a@ is the result when the API runs.
type APIT s e m a = EitherT (APIError e) (ReaderT Manager (StateT Builder (StateT s m))) a

-- | Lifts an action that works on an @API@ to an action that works on an @API@.
--   This function is provided solely for future-proofing in the case that more transformers
--   need to be stacked on top - it's implemented simply as @id@ for the moment.
liftEither :: Monad m => EitherT (APIError e) (ReaderT Manager (StateT Builder (StateT s m))) a -> APIT s e m a
liftEither = id

-- | Lifts an action that works on a @Manager@ to one that works on an @API@.
liftManager :: Monad m => ReaderT Manager (StateT Builder (StateT s m)) a -> APIT s e m a
liftManager = lift

-- | Lifts an action that operates on a @Builder@ to one that works on an @API@. Useful
--   mainly for gaining access to a @Builder@ from inside an @API@.
liftBuilder :: Monad m => StateT Builder (StateT s m) a -> APIT s e m a
liftBuilder = lift . lift

-- | Lifts an action on an @API@'s state type @s@ to one that works on the @API@. Good
--   for messing with the state from inside the @API@.
liftState :: Monad m => StateT s m a -> APIT s e m a
liftState = lift . lift . lift

-- | Runs an @API@ by executing its transformer stack and dumping it all into @IO@. Only returns the actual result.
execAPI :: MonadIO m
       => Builder -- ^ initial @Builder@ for the @API@
       -> s -- ^ initial state @s@ for the @API@
       -> APIT s e m a -- ^ the actual @API@ to run
       -> m (Either (APIError e) a) -- ^ IO action that returns either an error or the result
execAPI b s api = do
  m <- liftIO $ newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
  (res, _, _) <- runAPI b m s api
  liftIO $ closeManager m
  return res

-- | Runs an @API@ by executing its transformer stack and dumping it all into @IO@. Returns the actual result as well as the final states of the @Builder@ and custom state @s@.
runAPI :: MonadIO m
       => Builder -- ^ initial @Builder@ for the @API@
       -> Manager -- ^ manager for working with conduit functions
       -> s -- ^ initial state @s@ for the @API@
       -> APIT s e m a -- ^ the actual @API@ to run
       -> m (Either (APIError e) a, Builder, s) -- ^ IO action that returns either an error or the result, as well as the final states
runAPI b m s api = do
  ((res, b'), s') <- runStateT (runStateT (runReaderT (runEitherT api) m) b) s
  return (res, b', s')

-- | Runs a @Route@. Infers the type to convert to from the JSON with the @a@ in @API@,
--   and infers the error type from @e@.
runRoute :: (Receivable a, ErrorReceivable e, MonadIO m) => Route -> APIT s e m a
runRoute = sendRoute ()

-- | Runs a @Route@, but only returns the response and does nothing towards
--   decoding the response.
routeResponse :: (MonadIO m, ErrorReceivable e) => Route -> APIT s e m (Response ByteString)
routeResponse = sendRoute ()

eitherOr :: Maybe a -> b -> Either b a
a `eitherOr` b =
  case a of
    Just x -> Right x
    Nothing -> Left b

sendRoute :: (MonadIO m, Sendable t, ErrorReceivable e, Receivable r) => t -> Route -> APIT s e m r
sendRoute s r = do
  builder <- liftBuilder get
  manager <- liftManager ask
  req <- hoistEither $ send builder r s `eitherOr` InvalidURLError
  response <- liftIO $ try $ httpLbs req manager
  res <- hoistEither $ first HTTPError response
  hoistEither $ receive res

-- | Try to construct a @Request@ from a @Route@ (with the help of the @Builder@). Returns @Nothing@ if
--   the URL is invalid or there is another error with the @Route@.
routeRequest :: Builder -> Route -> Maybe Request
routeRequest b route = send b route ()

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
