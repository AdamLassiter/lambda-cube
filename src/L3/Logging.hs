{-# LANGUAGE FlexibleContexts #-}
module L3.Logging (module L3.Logging,
                   LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError),
                   withStderrLogging, withStdoutLogging, withFileLogging,
                   setLogLevel) where
    import Data.Text (pack)
    import GHC.IO (unsafeDupablePerformIO)

    import qualified Control.Logging
    import Control.Logging (LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError),
                            withStderrLogging, withStdoutLogging, withFileLogging,
                            setLogLevel)
    import Control.Monad.Trans.Control (MonadBaseControl)
    import Control.Monad.IO.Class (MonadIO)


    logId :: (String -> IO ()) -> String -> a -> IO a
    logId logger msg a = do
        _ <- logger msg
        pure a

    traceU = Control.Logging.traceL . pack
    debugM = Control.Logging.debug . pack
    debugU msg = unsafeDupablePerformIO . logId debugM msg
    infoM = Control.Logging.log . pack
    infoU msg = unsafeDupablePerformIO . logId infoM msg
    warnM = Control.Logging.warn . pack
    warnU msg = unsafeDupablePerformIO . logId warnM msg
    errorU = Control.Logging.errorL . pack
