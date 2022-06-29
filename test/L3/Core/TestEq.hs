module L3.Core.TestEq (tests) where

import L3.Core
import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testAlphaEq,
    testBetaEq
  ]

testAlphaEq :: IO ()
testAlphaEq = do
  assertAlphaEq "Core::TestEq" (Var (Name "x")) (Var (Name "x")) "Matching unbound names are alpha-equivalent"
  assertNotAlphaEq "Core::TestEq" (Var (Name "x")) (Var (Name "y")) "Non-matching unbound names are not alpha-equivalent"

  assertAlphaEq "Core::TestEq" (Lam (Name "x") Star (Var (Name "x"))) (Lam (Name "y") Star (Var (Name "y"))) "Bound names are alpha-equivalent over lambdas"
  assertAlphaEq "Core::TestEq" (Pi (Name "x") Star (Var (Name "x"))) (Pi (Name "y") Star (Var (Name "y"))) "Bound names are alpha-equivalent over pis"

  assertAlphaEq "Core::TestEq" (Lam (Name "x") Star (Var (Name "x")) `App` Var (Name "y")) (Lam (Name "y") Star (Var (Name "y")) `App` Var (Name "y")) "Alpha-equivalence distributes over application"

testBetaEq :: IO ()
testBetaEq =
  assertBetaEq "Core::TestEq" (Lam (Name "A") Star (Lam (Name "X") (Var (Name "A")) (Lam (Name "a") Star (Lam (Name "x") (Var (Name "a")) (Var (Name "x"))) `App` Var (Name "A") `App` Var (Name "X")))) (Lam (Name "a") Star (Lam (Name "x") (Var (Name "a")) (Var (Name "x")))) "Id Id is equivalent to Id"
