-- | Safe and unsafe logging functions wrapping Control.Logging
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


    logId :: (String -> String -> IO ()) -> String -> String -> a -> IO a
    logId logger src msg a = do
        _ <- logger src msg
        pure a

    traceU src msg = Control.Logging.traceSL  (pack src) (pack msg)
    debugM src msg = Control.Logging.debugS (pack src) (pack msg)
    debugU src msg = unsafeDupablePerformIO . logId debugM src msg
    infoM src msg = Control.Logging.logS  (pack src) (pack msg)
    infoU src msg = unsafeDupablePerformIO . logId infoM src msg
    warnM src msg = Control.Logging.warnS (pack src) (pack msg)
    warnU src msg = unsafeDupablePerformIO . logId warnM src msg
    errorU src msg = Control.Logging.errorSL (pack src) (pack msg)
