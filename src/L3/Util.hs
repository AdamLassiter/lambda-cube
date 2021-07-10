-- Utilites for result types and error throwing
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

    -- Unsafe variants of logger functions
    {-# NOINLINE logU #-}
    {-# NOINLINE debugU #-}
    {-# NOINLINE infoU #-}
    {-# NOINLINE noticeU #-}
    {-# NOINLINE warningU #-}
    {-# NOINLINE errorU #-}
    {-# NOINLINE criticalU #-}
    {-# NOINLINE alertU #-}
    {-# NOINLINE emergencyU #-}
    logU = logM . unsafePerformIO
    debugU = debugM . unsafePerformIO
    infoU = infoM . unsafePerformIO
    noticeU = noticeM . unsafePerformIO
    warningU = warningM . unsafePerformIO
    errorU = errorM . unsafePerformIO
    criticalU = criticalM . unsafePerformIO
    alertU = alertM . unsafePerformIO
    emergencyU = emergencyM . unsafePerformIO
