module Test (module Test) where

import L3.Core
import L3.Log
import L3.Parse
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

assertAlphaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertAlphaEq src = assert src (alphaEq, "=α=")

assertNotAlphaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertNotAlphaEq src = assert src ((not .) . alphaEq, "=α=")

assertBetaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertBetaEq src = assert src (betaEq, "=β=")

assertNotBetaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertNotBetaEq src = assert src ((not .) . betaEq, "=β=")

showSome :: (Show a, Show b) => [(a, b)] -> String
showSome [] = "[  ]"
showSome x = "[" ++ show (fst $ head x) ++ " .. [+" ++ show (length x - 2) ++ " elements] .. " ++ show (fst $ last x) ++ "]"

assertListEq :: (Eq a, Show a, Eq b, Show b) => String -> [(a, b)] -> [(a, b)] -> String -> IO ()
assertListEq src = assertShowing src showSome ((==), "==")

assertListNotEmpty :: (Eq a, Show a, Eq b, Show b) => String -> [(a, b)] -> String -> IO ()
assertListNotEmpty src x = assertShowing src showSome ((/=), "/=") x []

assertNormalizedEq :: (Eq a, Show a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertNormalizedEq src = assertShowing src (showExpr . normalize0) (betaEq, "=β=")

assertError :: String -> Result a -> String -> IO ()
assertError src x = assertTrue src $ isError x
