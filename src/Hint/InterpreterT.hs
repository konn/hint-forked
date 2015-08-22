module Hint.InterpreterT (
    InterpreterT, Interpreter, runInterpreter, runInterpreterWithArgs,
    MultipleInstancesNotAllowed(..)
)

where

import Prelude

import Hint.Base
import Hint.Context
import Hint.Configuration
import Hint.Extension

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Catch as MC

import Data.Typeable ( Typeable )
import Control.Concurrent.MVar
import System.IO.Unsafe ( unsafePerformIO )

import Data.IORef
import Data.List
import Data.Maybe
#if __GLASGOW_HASKELL__ < 610
import Data.Dynamic
#endif

import qualified GHC.Paths

import qualified Hint.GHC as GHC
import qualified Hint.Compat as Compat

type Interpreter = InterpreterT IO

#if __GLASGOW_HASKELL__ < 610

newtype InterpreterT m a = InterpreterT{
                             unInterpreterT :: ReaderT InterpreterSession
                                               (ErrorT InterpreterError m) a}
    deriving (Functor, Monad, MonadIO, MonadThrow,MonadCatch,MonadMask)

execute :: (MonadIO m, MonadMask m, Functor m)
        => InterpreterSession
        -> InterpreterT m a
        -> m (Either InterpreterError a)
execute s = runErrorT . flip runReaderT s . unInterpreterT

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift

runGhc_impl :: (MonadIO m, MonadThrow m, MonadMask m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl f = do s <- fromSession versionSpecific -- i.e. the ghc session
                   r <- liftIO $ f' s
                   either throwError return r
    where f' = tryJust (fmap (GhcException . showGhcEx) . ghcExceptions) . f
          ghcExceptions (DynException e) = fromDynamic e
          ghcExceptions  _               = Nothing

#else
      -- ghc >= 6.10
newtype InterpreterT m a = InterpreterT{
                             unInterpreterT :: ReaderT  InterpreterSession
                                              (GHC.GhcT m) a}
    deriving (Functor, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

execute :: (MonadIO m, MonadMask m, Functor m)
        => InterpreterSession
        -> InterpreterT m a
        -> m (Either InterpreterError a)
execute s = try
          . GHC.runGhcT (Just GHC.Paths.libdir)
          . flip runReaderT s
          . unInterpreterT


instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift

runGhc_impl :: (MonadIO m, MonadThrow m, MonadMask m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl a =
  InterpreterT (lift a)
   `catches`
   [Handler (\(e :: GHC.SourceError)  -> do
     dynFlags <- runGhc GHC.getSessionDynFlags
     throwM $ compilationError dynFlags e)
   ,Handler (\(e :: GHC.GhcApiError)  -> throwM $ GhcException $ show e)
   ,Handler (\(e :: GHC.GhcException) -> throwM $ GhcException $ showGhcEx e)
   ]
  where
    compilationError dynFlags
      = WontCompile
#if __GLASGOW_HASKELL__ >= 706
      . map (GhcError . GHC.showSDoc dynFlags)
#else
      . map (GhcError . GHC.showSDoc)
#endif
      . GHC.pprErrMsgBagWithLoc
      . GHC.srcErrorMessages
#endif

showGhcEx :: GHC.GhcException -> String
showGhcEx = flip GHC.showGhcException ""

-- ================= Executing the interpreter ==================

initialize :: (MonadIO m, MonadThrow m, MonadMask m, Functor m)
              => [String]
              -> InterpreterT m ()
initialize args =
    do log_handler <- fromSession ghcErrLogger
       -- Set a custom log handler, to intercept error messages :S
       df0 <- runGhc GHC.getSessionDynFlags

       let df1 = Compat.configureDynFlags df0
       (df2, extra) <- runGhc2 Compat.parseDynamicFlags df1 args
       when (not . null $ extra) $
            throwM $ UnknownError (concat [ "flags: '"
                                          , intercalate " " extra
                                          , "' not recognized"])

       -- Observe that, setSessionDynFlags loads info on packages
       -- available; calling this function once is mandatory!
       _ <- runGhc1 GHC.setSessionDynFlags df2{GHC.log_action = log_handler}

#if __GLASGOW_HASKELL__ >= 700
#if __GLASGOW_HASKELL__ >= 702
#if __GLASGOW_HASKELL__ >= 710
       let extMap      = map (\fs -> (GHC.flagSpecName fs, GHC.flagSpecFlag fs)) GHC.xFlags
#elif __GLASGOW_HASKELL__ >= 704
       let extMap      = map (\(a,b,_) -> (a,b)) GHC.xFlags
#else
       let extMap      = map (\(a,_,b,_) -> (a,b)) GHC.xFlags
#endif
#else
       let extMap      = map (\(a,b,_) -> (a,b)) GHC.xFlags
#endif
       let toOpt e     = let err = error ("init error: unknown ext:" ++ show e)
                         in fromMaybe err (lookup e extMap)
       let getOptVal e = (asExtension e, GHC.xopt (toOpt e) df2)
       let defExts = map  getOptVal Compat.supportedExtensions
#else
       let defExts = zip availableExtensions (repeat False)
#endif

       onState (\s -> s{defaultExts = defExts})

       reset


-- | Executes the interpreter. Returns @Left InterpreterError@ in case of error.
--
-- NB. The underlying ghc will overwrite certain signal handlers
-- (SIGINT, SIGHUP, SIGTERM, SIGQUIT on Posix systems, Ctrl-C handler on Windows).
-- In future versions of hint, this might be controlled by the user.
runInterpreter :: (MonadIO m, MonadMask m, Functor m)
               => InterpreterT m a
               -> m (Either InterpreterError a)
runInterpreter = runInterpreterWithArgs []

-- | Executes the interpreter, setting args passed in as though they
-- were command-line args. Returns @Left InterpreterError@ in case of
-- error.
runInterpreterWithArgs :: (MonadIO m, MonadMask m, Functor m)
                          => [String]
                          -> InterpreterT m a
                          -> m (Either InterpreterError a)
runInterpreterWithArgs args action =
  ifInterpreterNotRunning $
    do s <- newInterpreterSession `MC.catch` rethrowGhcException
       -- SH.protectHandlers $ execute s (initialize args >> action)
       execute s (initialize args >> action `finally` cleanSession)
    where rethrowGhcException   = throwM . GhcException . showGhcEx
#if __GLASGOW_HASKELL__ < 610
          newInterpreterSession =  do s <- liftIO $
                                             Compat.newSession GHC.Paths.libdir
                                      newSessionData s
          cleanSession = cleanPhantomModules -- clean ghc session, too?
#else
          -- GHC >= 610
          newInterpreterSession = newSessionData ()
          cleanSession =
               do cleanPhantomModules
                  runGhc $ do dflags <- GHC.getSessionDynFlags
                              GHC.defaultCleanupHandler dflags (return ())
#endif

{-# NOINLINE uniqueToken #-}
uniqueToken :: MVar ()
uniqueToken = unsafePerformIO $ newMVar ()

ifInterpreterNotRunning :: (MonadIO m, MonadMask m) => m a -> m a
ifInterpreterNotRunning action =
    do maybe_token <- liftIO $ tryTakeMVar uniqueToken
       case maybe_token of
           Nothing -> throwM MultipleInstancesNotAllowed
           Just x  -> action `finally` (liftIO $ putMVar uniqueToken x)

-- | The installed version of ghc is not thread-safe. This exception
--   is thrown whenever you try to execute @runInterpreter@ while another
--   instance is already running.
data MultipleInstancesNotAllowed = MultipleInstancesNotAllowed deriving Typeable

instance Exception MultipleInstancesNotAllowed

instance Show MultipleInstancesNotAllowed where
    show _ = "This version of GHC is not thread-safe," ++
             "can't safely run two instances of the interpreter simultaneously"

initialState :: InterpreterState
initialState = St {active_phantoms      = [],
                   zombie_phantoms      = [],
                   hint_support_module  = error "No support module loaded!",
                   import_qual_hack_mod = Nothing,
                   qual_imports         = [],
                   defaultExts          = error "defaultExts missing!",
                   configuration        = defaultConf}


newSessionData :: MonadIO m => a -> m (SessionData a)
newSessionData  a =
    do initial_state    <- liftIO $ newIORef initialState
       ghc_err_list_ref <- liftIO $ newIORef []
       return SessionData{
         internalState   = initial_state,
         versionSpecific = a,
         ghcErrListRef   = ghc_err_list_ref,
         ghcErrLogger    = mkLogHandler ghc_err_list_ref
       }

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r =
#if __GLASGOW_HASKELL__ < 706
  \_ src style msg ->
    let renderErrMsg = Compat.showSDoc ()
#else
  \df _ src style msg ->
    let renderErrMsg = Compat.showSDoc df
#endif
        errorEntry = mkGhcError renderErrMsg src style msg
    in modifyIORef r (errorEntry :)

mkGhcError :: (GHC.SDoc -> String) -> GHC.SrcSpan -> GHC.PprStyle -> GHC.Message -> GhcError
mkGhcError render src_span style msg = GhcError{errMsg = niceErrMsg}
    where niceErrMsg = render . GHC.withPprStyle style $
                         Compat.mkLocMessage src_span msg


-- The MonadInterpreter instance

instance (MonadIO m, MonadMask m, Functor m) => MonadInterpreter (InterpreterT m) where
    fromSession f = InterpreterT $ fmap f ask
    --
    modifySessionRef target f =
        do ref     <- fromSession target
           old_val <- liftIO $ atomicModifyIORef ref (\a -> (f a, a))
           return old_val
    --
    runGhc a = runGhc_impl a


instance (Monad m, Applicative m) => Applicative (InterpreterT m) where
    pure  = return
    (<*>) = ap
