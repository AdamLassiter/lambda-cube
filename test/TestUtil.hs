module TestUtil (module TestUtil) where

    assert :: (Show a) => (a -> a -> Bool, String) -> a -> a -> String -> IO ()
    assert (op, showOp) actual expected msg = do
        putStrLn msg
        if actual `op` expected
            then putStrLn $ "Success, true assertion for " ++ show expected ++ " " ++ showOp ++ " " ++ show actual
            else error $ "Error, false assertion for " ++ show expected ++ " " ++ showOp ++ " " ++ show actual
        putStrLn ""
        return ()

    assertEq :: (Eq a, Show a) => a -> a -> String -> IO ()
    assertEq = assert ((==), "==")

    assertTrue :: Bool -> String -> IO ()
    assertTrue = (`assertEq` True)

    assertFalse :: Bool -> String -> IO ()
    assertFalse = (`assertEq` False)
