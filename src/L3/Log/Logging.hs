{-# LANGUAGE CPP #-}

-- | Safe and unsafe logging functions wrapping Control.Logging
module L3.Log.Logging
  ( module L3.Log.Logging,
    LogLevel (LevelDebug, LevelInfo, LevelWarn, LevelError),
    withStderrLogging,
    withStdoutLogging,
    withFileLogging,
    setLogLevel,
    setDebugSourceRegex,
  )
where

import Control.Logging
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
    setDebugSourceRegex,
    setLogLevel,
    withFileLogging,
    withStderrLogging,
    withStdoutLogging,
  )
import qualified Control.Logging
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (pack)
import GHC.IO (unsafeDupablePerformIO)
import L3.Log.Colors

logId :: (String -> String -> IO ()) -> String -> String -> a -> IO a
logId logger src msg a = do
  _ <- logger src msg
  pure a

traceU :: String -> String -> a -> a
#ifdef LOGTRACE
#define LOGDEBUG
traceU src msg = Control.Logging.traceSL (pack $ brightWhite ++ src ++ reset) (pack msg)

#else

traceU _ _ = id
#endif

debugM :: String -> String -> IO ()
traceU :: String -> String -> a -> a
#ifdef LOGDEBUG
#define LOGINFO
debugM src msg = Control.Logging.debugS (pack $ blue ++ src ++ reset) (pack msg)

traceU src msg = unsafeDupablePerformIO . logId debugM src msg

#else

debugM _ _ = return ()

traceU _ _ = id
#endif

infoM :: String -> String -> IO ()
infoU :: String -> String -> a -> a
#ifdef LOGINFO
#define LOGWARN
infoM src msg = Control.Logging.logS (pack $ green ++ src ++ reset) (pack msg)

infoU src msg = unsafeDupablePerformIO . logId infoM src msg

#else

infoM _ _ = return ()

infoU _ _ = id
#endif

warnM :: String -> String -> IO ()
warnU :: String -> String -> a -> a
#ifdef LOGWARN
warnM src msg  = Control.Logging.warnS (pack $ yellow ++ src ++ reset) (pack msg)
warnU src msg  = unsafeDupablePerformIO . logId warnM src msg
#else
warnM _ _  = return ()
warnU _ _  = id
#endif

errorU :: String -> String -> a
errorU src msg = Control.Logging.errorSL (pack $ red ++ src ++ reset) (pack msg)
