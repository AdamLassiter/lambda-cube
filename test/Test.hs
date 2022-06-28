module Test (module Test) where

import L3.Logging
import L3.Util

assertShowing :: String -> (a -> String) -> (a -> a -> Bool, String) -> a -> a -> String -> IO ()
assertShowing src show' (op, showOp) actual expected msg = do
  let detail = "\n\t" ++ "expected: " ++ show' expected ++ "\n\t" ++ "to be: " ++ showOp ++ "\n\t" ++ "actual:   " ++ show' actual
  if actual `op` expected
    then do
      infoM src msg
      infoM src $ "Success, true assertion:" ++ detail
    else do
      warnM src msg
      errorU src $ "Error, false assertion:" ++ detail
  return ()

assert :: (Show a) => String -> (a -> a -> Bool, String) -> a -> a -> String -> IO ()
assert src = assertShowing src show

assertEq :: (Eq a, Show a) => String -> a -> a -> String -> IO ()
assertEq src = assert src ((==), "==")

assertTrue :: String -> Bool -> String -> IO ()
assertTrue src = (`assertEq0` True)
  where
    assertEq0 = assertEq src

assertFalse :: String -> Bool -> String -> IO ()
assertFalse src = (`assertEq0` False)
  where
    assertEq0 = assertEq src
