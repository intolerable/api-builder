module Network.API.Builder.API (
  -- * API
    API
  , APIT
  , ResumeAPIT
  -- ** Running the API
  , execAPI
  , runAPI
  , runResumeAPI
  , runRoute
  , sendRoute
  , routeResponse
  , routeRequest
  -- ** Lifting
  , liftExcept
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
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- | Main API type. @s@ is the API's internal state, @e@ is the API's custom error type,
--   and @a@ is the result when the API runs. Based on the @APIT@ transformer.
type API s e a = APIT s e IO a

type APIT s e m a = ResumeAPIT s e a m a

-- | Main API transformer type. @s@ is the API's internal state, @e@ is the API's custom error type,
--   and @a@ is the result when the API runs.
newtype ResumeAPIT s e r m a =
  ResumeAPIT (ContT r (ExceptT (APIError e, Maybe (ResumeAPIT s e r m r)) (ReaderT Manager (StateT Builder (StateT s m)))) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ResumeAPIT s e r) where
  lift = ResumeAPIT . lift . lift . lift . lift . lift

-- | Lifts an action that works on an @API@ to an action that works on an @API@.
--   This function is provided solely for future-proofing in the case that more transformers
--   need to be stacked on top - it's implemented simply as @id@ for the moment.
liftExcept :: Monad m => ExceptT (APIError e, Maybe (ResumeAPIT s e r m r)) (ReaderT Manager (StateT Builder (StateT s m))) a -> ResumeAPIT s e r m a
liftExcept = ResumeAPIT . lift

{-# DEPRECATED liftEither "Use liftExcept" #-}
-- | Identical to 'liftExcept', provided for (almost) compatibility.
liftEither :: Monad m => ExceptT (APIError e, Maybe (ResumeAPIT s e r m r)) (ReaderT Manager (StateT Builder (StateT s m))) a -> ResumeAPIT s e r m a
liftEither = ResumeAPIT . lift

-- | Lifts an action that works on a @Manager@ to one that works on an @API@.
liftManager :: Monad m => ReaderT Manager (StateT Builder (StateT s m)) a -> ResumeAPIT s e r m a
liftManager = ResumeAPIT . lift . lift

-- | Lifts an action that operates on a @Builder@ to one that works on an @API@. Useful
--   mainly for gaining access to a @Builder@ from inside an @API@.
liftBuilder :: Monad m => StateT Builder (StateT s m) a -> ResumeAPIT s e r m a
liftBuilder = ResumeAPIT . lift . lift . lift

-- | Lifts an action on an @API@'s state type @s@ to one that works on the @API@. Good
--   for messing with the state from inside the @API@.
liftState :: Monad m => StateT s m a -> ResumeAPIT s e r m a
liftState = ResumeAPIT . lift . lift . lift . lift

-- | Runs an @API@ by executing its transformer stack and dumping it all into @IO@. Only returns the actual result.
execAPI :: MonadIO m
       => Builder -- ^ initial @Builder@ for the @API@
       -> s -- ^ initial state @s@ for the @API@
       -> ResumeAPIT s e a m a -- ^ the actual @API@ to run
       -> m (Either (APIError e) a) -- ^ IO action that returns either an error or the result
execAPI b s api = do
  m <- liftIO $ newManager tlsManagerSettings
  (res, _, _) <- runAPI b m s api
  liftIO $ closeManager m
  return $ res

-- | Runs an @API@ by executing its transformer stack and dumping it all into @IO@.
-- | Returns the actual result as well as the final states of the @Builder@ and custom state @s@.
runAPI :: MonadIO m
       => Builder -- ^ initial @Builder@ for the @API@
       -> Manager -- ^ manager for working with conduit functions
       -> s -- ^ initial state @s@ for the @API@
       -> ResumeAPIT s e a m a -- ^ the actual @API@ to run
       -> m (Either (APIError e) a, Builder, s) -- ^ IO action that returns either an error or the result, as well as the final states
runAPI b m s (ResumeAPIT api) = do
  ((res, b'), s') <- runStateT (runStateT (runReaderT (runExceptT (runContT api return)) m) b) s
  return (bimap fst id res, b', s')

runResumeAPI :: MonadIO m
             => Builder
             -> Manager
             -> s
             -> ResumeAPIT s e a m a
             -> m (Either (APIError e, Maybe (ResumeAPIT s e a m a)) a, Builder, s)
runResumeAPI b m s (ResumeAPIT api) = do
  ((res, b'), s') <- runStateT (runStateT (runReaderT (runExceptT (runContT api return)) m) b) s
  return (res, b', s')

-- | Runs a @Route@. Infers the type to convert to from the JSON with the @a@ in @API@,
--   and infers the error type from @e@.
runRoute :: (Receivable a, ErrorReceivable e, MonadIO m) => Route -> ResumeAPIT s e r m a
runRoute = sendRoute ()

-- | Runs a @Route@, but only returns the response and does nothing towards
--   decoding the response.
routeResponse :: (MonadIO m, ErrorReceivable e) => Route -> ResumeAPIT s e r m (Response ByteString)
routeResponse = sendRoute ()

eitherOr :: Maybe a -> b -> Either b a
a `eitherOr` b =
  case a of
    Just x -> Right x
    Nothing -> Left b

sendRoute :: (MonadIO m, Sendable t, ErrorReceivable e, Receivable a) => t -> Route -> ResumeAPIT s e r m a
sendRoute s r = do
  builder <- liftBuilder get
  manager <- liftManager ask
  ResumeAPIT $ do
    label <- fmap ResumeAPIT $ mkLabel
    lift $ mapExceptT (fmap $ addLabel label) $ do
      req <- ExceptT $ return $ send builder r s `eitherOr` InvalidURLError
      response <- liftIO $ try $ httpLbs req manager
      res <- ExceptT $ return $ first HTTPError response
      ExceptT $ return $ receive res

addLabel :: c -> Either a b -> Either (a, Maybe c) b
addLabel l (Left x) = Left (x, Just l)
addLabel _ (Right x) = Right x

mkLabel :: ContT r m (ContT r m a)
mkLabel = callCC $ \k -> let x = k x in return x

-- | Try to construct a @Request@ from a @Route@ (with the help of the @Builder@). Returns @Nothing@ if
--   the URL is invalid or there is another error with the @Route@.
routeRequest :: Builder -> Route -> Maybe Request
routeRequest b route = send b route ()

-- | Modify the @name@ of the @Builder@ from inside an API. Using this is probably not the best idea,
--   it's nice if the @Builder@'s name is stable at least.
name :: Monad m => Text -> ResumeAPIT s e r m ()
name t = liftBuilder $ modify (\b -> b { _name = t })

-- | Modify the @baseURL@ of the @Builder@ from inside an API.
--   Can be useful for changing the API's endpoints for certain requests.
baseURL :: Monad m => Text -> ResumeAPIT s e r m ()
baseURL t = liftBuilder $ modify (\b -> b { _baseURL = t })

-- | Modify every @Route@ before it runs. Useful for adding extra params to every query,
--   for example.
customizeRoute :: Monad m => (Route -> Route) -> ResumeAPIT s e r m ()
customizeRoute f = liftBuilder $ modify (\b -> b { _customizeRoute = f })

-- | Modify every @Request@ before the API fetches it. Useful for adding headers to every request,
--   for example.
customizeRequest :: Monad m => (Request -> Request) -> ResumeAPIT s e r m ()
customizeRequest f = liftBuilder $ modify (\b -> b { _customizeRequest = f })
