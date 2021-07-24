module TestUtil (module TestUtil) where
    import L3.Logging
    import L3.Util


    assertShowing :: (a -> String) -> (a -> a -> Bool, String) -> a -> a -> String -> IO ()
    assertShowing show' (op, showOp) actual expected msg = do
        if actual `op` expected
            then do
              infoM msg
              infoM $ "Success, true assertion for " ++ show' expected ++ " " ++ showOp ++ " " ++ show' actual
            else do
              warnM msg
              errorU $ "Error, false assertion for " ++ show' expected ++ " " ++ showOp ++ " " ++ show' actual
        return ()

    assert :: (Show a) => (a -> a -> Bool, String) -> a -> a -> String -> IO ()
    assert = assertShowing show

    assertEq :: (Eq a, Show a) => a -> a -> String -> IO ()
    assertEq = assert ((==), "==")

    assertTrue :: Bool -> String -> IO ()
    assertTrue = (`assertEq` True)

    assertFalse :: Bool -> String -> IO ()
    assertFalse = (`assertEq` False)
