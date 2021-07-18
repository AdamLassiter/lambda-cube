-- | Utilites for result types and error throwing
module L3.Util where
    import System.Log.Logger
    import GHC.IO (unsafePerformIO)
    type Result a = Either String a


    throwError :: String -> Result a
    throwError = Left

    unpack :: [Result a] -> Result [a]
    unpack (Left err:_) = throwError err
    unpack (Right r:rs) = case unpack rs of
        Left err  -> throwError err
        Right rs' -> Right (r:rs')
    unpack []           = Right []

    mapL :: (String -> String) -> Result a -> Result a
    mapL f (Left err)  = Left $ f err
    mapL _ (Right res) = Right res

    mapR :: (a -> b) -> Result a -> Result b
    mapR _ (Left err)  = Left err
    mapR f (Right res) = Right $ f res

    fmapR :: (a -> Result b) -> Result a -> Result b
    fmapR _ (Left err)  = Left err
    fmapR f (Right res) = f res

    flatten :: Result (Result a) -> Result a
    flatten (Left err)          = Left err
    flatten (Right (Left err))  = Left err
    flatten (Right (Right res)) = Right res

    throwL :: Result a -> a
    throwL (Left err)  = error err
    throwL (Right res) = res

    converge :: Eq a => (a -> a) -> a -> a
    converge = until =<< ((==) =<<)


    {-# NOINLINE logU #-}
    -- | Unsafe log to logger-name at priority-level with log-message
    logU :: String -> Priority -> String -> ()
    logU = logM . unsafePerformIO

    {-# NOINLINE debugU #-}
    -- | Unsafe debug-log to logger-name with log-message
    debugU :: String -> String -> ()
    debugU = debugM . unsafePerformIO

    {-# NOINLINE infoU #-}
    -- | Unsafe info-log to logger-name with log-message
    infoU :: String -> String -> ()
    infoU = infoM . unsafePerformIO

    {-# NOINLINE noticeU #-}
    -- | Unsafe notice-log to logger-name with log-message
    noticeU :: String -> String -> ()
    noticeU = noticeM . unsafePerformIO

    {-# NOINLINE warningU #-}
    -- | Unsafe warning-log to logger-name with log-message
    warningU :: String -> String -> ()
    warningU = warningM . unsafePerformIO

    {-# NOINLINE errorU #-}
    -- | Unsafe error-log to logger-name with log-message
    errorU :: String -> String -> ()
    errorU = errorM . unsafePerformIO

    {-# NOINLINE criticalU #-}
    -- | Unsafe critical-log to logger-name with log-message
    criticalU :: String -> String -> ()
    criticalU = criticalM . unsafePerformIO

    {-# NOINLINE alertU #-}
    -- | Unsafe alert-log to logger-name with log-message
    alertU :: String -> String -> ()
    alertU = alertM . unsafePerformIO

    {-# NOINLINE emergencyU #-}
    -- | Unsafe emergency-log to logger-name with log-message
    emergencyU :: String -> String -> ()
    emergencyU = emergencyM . unsafePerformIO
