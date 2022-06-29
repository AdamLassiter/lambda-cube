module L3.Core.TestDeBruijn (tests) where

import L3.Core
import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testInstanceEnum,
    testEnumInstance,
    testIndex
  ]

testInstanceEnum :: IO ()
testInstanceEnum = do
  assertEq "Core::TestDeBruijn" ((toEnum . fromEnum) (Left 1 :: Either Int Int)) (Left 1 :: Either Int Int) "Instance preserves forward invariant for left Enum"
  assertEq "Core::TestDeBruijn" ((toEnum . fromEnum) (Right 1 :: Either Int Int)) (Right 1 :: Either Int Int) "Instance preserves forward invariant for right Enum"

testEnumInstance :: IO ()
testEnumInstance = do
  assertEq "Core::TestDeBruijn" ((toEnum . fromEnum) (Left 1 :: Either Int Int)) (Left 1 :: Either Int Int) "Instance preserves backward invariant for left Enum"
  assertEq "Core::TestDeBruijn" ((toEnum . fromEnum) (Right 1 :: Either Int Int)) (Right 1 :: Either Int Int) "Instance preserves backward invariant for right Enum"

testIndex :: IO ()
testIndex = do
  assertEq "Core::TestDeBruijn" (index0 (Star :: ShowExpr)) Star "Star indexes to star"
  assertEq "Core::TestDeBruijn" (index0 (Box :: ShowExpr)) Box "Box indexes to box"

  assertEq "Core::TestDeBruijn" (index0 (Var (Name "x"))) (Var $ Right (Name "x")) "Unbound names cannot be indexed"

  assertEq "Core::TestDeBruijn" (index0 $ Lam (Name "x") Star (Var (Name "x"))) (Lam (Left 0) Star (Var $ Left 0)) "Bound names can be indexed"
  assertEq "Core::TestDeBruijn" (index0 $ Lam (Name "x") (Var (Name "x")) (Var (Name "x"))) (Lam (Left 0) (Var $ Right (Name "x")) (Var $ Left 0)) "Types are unbound relative to their binding"

  assertEq "Core::TestDeBruijn" (index0 $ Pi (Name "x") Star (Var (Name "x"))) (Pi (Left 0) Star (Var $ Left 0)) "Bound names can be indexed"
  assertEq "Core::TestDeBruijn" (index0 $ Pi (Name "x") (Var (Name "x")) (Var (Name "x"))) (Pi (Left 0) (Var $ Right (Name "x")) (Var $ Left 0)) "Types are unbound relative to their binding"

  assertEq "Core::TestDeBruijn" (index0 $ Var (Name "x") `App` Var (Name "y")) (Var (Right (Name "x")) `App` Var (Right (Name "y"))) "Unbound names cannot be indexed over applications"
  assertEq "Core::TestDeBruijn" (index0 $ Lam (Name "x") Star (Var (Name "x")) `App` Lam (Name "y") Star (Var (Name "y"))) (Lam (Left 0) Star (Var $ Left 0) `App` Lam (Left 0) Star (Var $ Left 0)) "Bound names can be indexed over applications"