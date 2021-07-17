module TestUtil (module TestUtil) where

    assertShowing :: (a -> String) -> (a -> a -> Bool, String) -> a -> a -> String -> IO ()
    assertShowing show' (op, showOp) actual expected msg = do
        putStrLn msg
        if actual `op` expected
            then putStrLn $ "Success, true assertion for " ++ show' expected ++ " " ++ showOp ++ " " ++ show' actual
            else error $ "Error, false assertion for " ++ show' expected ++ " " ++ showOp ++ " " ++ show' actual
        putStrLn ""
        return ()

    assert :: (Show a) => (a -> a -> Bool, String) -> a -> a -> String -> IO ()
    assert = assertShowing show

    assertEq :: (Eq a, Show a) => a -> a -> String -> IO ()
    assertEq = assert ((==), "==")

    assertTrue :: Bool -> String -> IO ()
    assertTrue = (`assertEq` True)

    assertFalse :: Bool -> String -> IO ()
    assertFalse = (`assertEq` False)
