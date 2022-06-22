{-# LANGUAGE CPP #-}

-- |Safe and unsafe logging functions wrapping Control.Logging
module L3.Logging (module L3.Logging,
                   LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError),
                   withStderrLogging, withStdoutLogging, withFileLogging,
                   setLogLevel, setDebugSourceRegex) where
    import Data.Text (pack)
    import GHC.IO (unsafeDupablePerformIO)

    import qualified Control.Logging
    import Control.Logging (LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError),
                            withStderrLogging, withStdoutLogging, withFileLogging,
                            setLogLevel, setDebugSourceRegex)
    import Control.Monad.Trans.Control (MonadBaseControl)
    import Control.Monad.IO.Class (MonadIO)

    reset = "\x001b[0m"
    black = "\x001b[30m"
    red = "\x001b[31m"
    green = "\x001b[32m"
    yellow = "\x001b[33m"
    blue = "\x001b[34m"
    magenta = "\x001b[35m"
    cyan = "\x001b[36m" 
    white = "\x001b[37m"
    brightBlack = "\x001b[30;1m"
    brightRed = "\x001b[31;1m"
    brightGreen = "\x001b[32;1m"
    brightYellow = "\x001b[33;1m"
    brightBlue = "\x001b[34;1m"
    brightMagenta = "\x001b[35;1m"
    brightCyan = "\x001b[36;1m"
    brightWhite = "\x001b[37;1m"


    logId :: (String -> String -> IO ()) -> String -> String -> a -> IO a
    logId logger src msg a = do
        _ <- logger src msg
        pure a

    debugM :: String -> String -> IO ()
    debugM src msg = Control.Logging.debugS (pack $ blue ++ src ++ reset) (pack msg)
    infoM :: String -> String -> IO ()
    infoM src msg  = Control.Logging.logS (pack $ green ++ src ++ reset) (pack msg)
    warnM :: String -> String -> IO ()
    warnM src msg  = Control.Logging.warnS (pack $ yellow ++ src ++ reset) (pack msg)
    errorU :: String -> String -> a
    errorU src msg = Control.Logging.errorSL (pack $ red ++ src ++ reset) (pack msg)

    traceU :: String -> String -> a -> a
    debugU :: String -> String -> a -> a
    infoU :: String -> String -> a -> a
    warnU :: String -> String -> a -> a

#ifdef LOGGING
    traceU src msg = Control.Logging.traceSL (pack src) (pack msg)
    debugU src msg = unsafeDupablePerformIO . logId debugM src msg
    infoU src msg  = unsafeDupablePerformIO . logId infoM src msg
    warnU src msg  = unsafeDupablePerformIO . logId warnM src msg
#else
    traceU _ _ = id
    debugU _ _ = id
    infoU _ _  = id
    warnU _ _  = id
#endif
