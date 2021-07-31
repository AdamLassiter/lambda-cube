module TestUtil (module TestUtil) where
    import L3.Logging
    import L3.Util

    infoTestUtil = infoM "TestUtil"
    warnTestUtil = warnM "TestUtil"
    errorTestUtil = errorU "TestUtil"


    assertShowing :: (a -> String) -> (a -> a -> Bool, String) -> a -> a -> String -> IO ()
    assertShowing show' (op, showOp) actual expected msg = do
        if actual `op` expected
            then do
              infoTestUtil msg
              infoTestUtil $ "Success, true assertion for " ++ show' expected ++ " " ++ showOp ++ " " ++ show' actual
            else do
              warnTestUtil msg
              errorTestUtil $ "Error, false assertion for " ++ show' expected ++ " " ++ showOp ++ " " ++ show' actual
        return ()

    assert :: (Show a) => (a -> a -> Bool, String) -> a -> a -> String -> IO ()
    assert = assertShowing show

    assertEq :: (Eq a, Show a) => a -> a -> String -> IO ()
    assertEq = assert ((==), "==")

    assertTrue :: Bool -> String -> IO ()
    assertTrue = (`assertEq` True)

    assertFalse :: Bool -> String -> IO ()
    assertFalse = (`assertEq` False)
