-- | Utilites for result types and error throwing
module L3.Util where
    import System.Log.Logger
    import GHC.IO (unsafePerformIO)

    newtype Error = Error ([String], Maybe Error)
        deriving (Eq)
    instance Show Error where
        show = show' 0
            where show' :: Int -> Error -> String
                  show' i (Error (errs, cause)) = unlines (showErrors i errs: showCause i cause)
                  showErrors i errs = unlines $ map (indent i) errs
                  indent i x = unwords $ replicate i "\t" ++ [x]
                  showCause i cause = case cause of
                      Just c -> [show' (i + 1) c]
                      Nothing -> []

    type Result a = Either Error a


    throw :: Error -> Result a
    throw = Left

    throwError :: [String] -> Error
    throwError errs = Error (errs, Nothing)

    rethrowError :: [String] -> Error -> Error
    rethrowError errs cause = Error (errs, Just cause)

    unpack :: [Result a] -> Result [a]
    unpack (Left err:_) = throw err
    unpack (Right r:rs) = case unpack rs of
        Left err  -> throw err
        Right rs' -> Right (r:rs')
    unpack []           = Right []

    mapL :: (Error -> Error) -> Result a -> Result a
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
    throwL (Left err)  = error $ show err
    throwL (Right res) = res

    converge :: Eq a => (a -> a) -> a -> a
    converge = until =<< ((==) =<<)


    {-# NOINLINE logU #-}
    -- | Unsafe log to logger-name at priority-level with log-message
    logU :: String -> Priority -> String -> ()
    logU name priority mesg = unsafePerformIO $ logM name priority mesg

    {-# NOINLINE debugU #-}
    -- | Unsafe debug-log to logger-name with log-message
    debugU :: String -> String -> ()
    debugU name mesg = unsafePerformIO $ debugM name mesg

    {-# NOINLINE infoU #-}
    -- | Unsafe info-log to logger-name with log-message
    infoU :: String -> String -> ()
    infoU name mesg = unsafePerformIO $ infoM name mesg

    {-# NOINLINE noticeU #-}
    -- | Unsafe notice-log to logger-name with log-message
    noticeU :: String -> String -> ()
    noticeU name mesg = unsafePerformIO $ noticeM name mesg

    {-# NOINLINE warningU #-}
    -- | Unsafe warning-log to logger-name with log-message
    warningU :: String -> String -> ()
    warningU name mesg = unsafePerformIO $ warningM name mesg

    {-# NOINLINE errorU #-}
    -- | Unsafe error-log to logger-name with log-message
    errorU :: String -> String -> ()
    errorU name mesg = unsafePerformIO $ errorM name mesg

    {-# NOINLINE criticalU #-}
    -- | Unsafe critical-log to logger-name with log-message
    criticalU :: String -> String -> ()
    criticalU name mesg = unsafePerformIO $ criticalM name mesg

    {-# NOINLINE alertU #-}
    -- | Unsafe alert-log to logger-name with log-message
    alertU :: String -> String -> ()
    alertU name mesg = unsafePerformIO $ alertM name mesg

    {-# NOINLINE emergencyU #-}
    -- | Unsafe emergency-log to logger-name with log-message
    emergencyU :: String -> String -> ()
    emergencyU name mesg = unsafePerformIO $ emergencyM name mesg
