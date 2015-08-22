module Hint.SignalHandlers (
    protectHandlers
)
where

import Control.Monad.Catch
import Control.Monad.Trans

#ifdef mingw32_HOST_OS
import GHC.ConsoleHandler as C

saveHandlers :: MonadIO m => m C.Handler
saveHandlers = liftIO $ C.installHandler Ignore

restoreHandlers :: MonadIO m => C.Handler -> m C.Handler
restoreHandlers = liftIO . C.installHandler

#else
import qualified System.Posix.Signals as S

helper :: MonadIO m => S.Handler -> S.Signal -> m S.Handler
helper handler signal = liftIO $ S.installHandler signal handler Nothing

signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]

saveHandlers :: MonadIO m => m [S.Handler]
saveHandlers = liftIO $ mapM (helper S.Ignore) signals

restoreHandlers :: MonadIO m => [S.Handler] -> m [S.Handler]
restoreHandlers h  = liftIO . sequence $ zipWith helper h signals

#endif

protectHandlers :: (MonadIO m, MonadMask m) => m a -> m a
protectHandlers a = bracket saveHandlers restoreHandlers $ const a
