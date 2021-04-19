-- Utilites for result types and error throwing
module L3.Util where

    type Result a = Either String a

    throwError :: String -> Result a
    throwError = Left

    unpack :: [Result a] -> Result [a]
    unpack (Left err:_) = throwError err
    unpack (Right r:rs) = case unpack rs of
        Left err  -> throwError err
        Right rs' -> Right (r:rs')

    mapR :: (a -> b) -> Result a -> Result b
    mapR f (Left err)  = Left err
    mapR f (Right res) = Right $ f res

    fmapR :: (a -> Result b) -> Result a -> Result b
    fmapR f (Left err)  = Left err
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
