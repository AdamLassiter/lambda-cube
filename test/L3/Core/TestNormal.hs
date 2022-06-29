module L3.Core.TestNormal (tests) where

import L3.Core
import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testFree,
    testSubstitute,
    testNormalize
  ]

testFree :: IO ()
testFree = do
  assertFalse "Core::TestNormal" (free 0 Star) "0 is not free in Star"
  assertFalse "Core::TestNormal" (free 0 Box) "0 is not free in Box"

  assertTrue "Core::TestNormal" (free 0 (Var 0)) "0 is free in Var 0"
  assertFalse "Core::TestNormal" (free 0 (Var 1)) "0 is not free in Var 1"

  assertFalse "Core::TestNormal" (free 0 (Lam 0 (Var 1) (Var 1))) "0 is not free in Lam 0 : 1 . 1"
  assertTrue "Core::TestNormal" (free 0 (Lam 0 (Var 0) (Var 1))) "0 is free in Lam 0 : 0 . 1"
  assertFalse "Core::TestNormal" (free 0 (Lam 0 (Var 1) (Var 0))) "0 is not free in Lam 0 : 1 . 0"
  assertTrue "Core::TestNormal" (free 0 (Lam 1 (Var 0) Star)) "0 is free in Lam 1 : 0 . *"
  assertTrue "Core::TestNormal" (free 0 (Lam 1 Star (Var 0))) "0 is free in Lam 1 : 0 . *"
  assertFalse "Core::TestNormal" (free 0 (Lam 1 (Var 1) (Var 1))) "0 is not free in Lam 1 : 1 . 1"

  assertFalse "Core::TestNormal" (free 0 (Pi 0 (Var 1) (Var 1))) "0 is not free in Pi 0 : 1 . 1"
  assertTrue "Core::TestNormal" (free 0 (Pi 0 (Var 0) (Var 1))) "0 is free in Pi 0 : 0 . 1"
  assertFalse "Core::TestNormal" (free 0 (Pi 0 (Var 1) (Var 0))) "0 is not free in Pi 0 : 1 . 0"
  assertTrue "Core::TestNormal" (free 0 (Pi 1 (Var 0) Star)) "0 is free in Pi 1 : 0 . *"
  assertTrue "Core::TestNormal" (free 0 (Pi 1 Star (Var 0))) "0 is free in Pi 1 : 0 . *"
  assertFalse "Core::TestNormal" (free 0 (Pi 1 (Var 1) (Var 1))) "0 is not free in Pi 1 : 1 . 1"

  assertFalse "Core::TestNormal" (free 0 (Star `App` Star)) "0 is not free in * *"
  assertTrue "Core::TestNormal" (free 0 (Var 0 `App` Star)) "0 is free in 0 *"
  assertTrue "Core::TestNormal" (free 0 (Star `App` Var 0)) "0 is free in * 0"
  assertTrue "Core::TestNormal" (free 0 (Var 0 `App` Var 0)) "0 is free in 0 0"

testSubstitute :: IO ()
testSubstitute = do
  assertEq "Core::TestNormal" (substitute 0 (Var 1) Star) Star "*[0 := 1] leaves * unchanged"
  assertEq "Core::TestNormal" (substitute 0 (Var 1) Box) Box "#[0 := 1] leaves # unchanged"

  assertEq "Core::TestNormal" (substitute 0 (Var 1) (Var 0)) (Var 1) "0[0 := 1] renames to 1"
  assertEq "Core::TestNormal" (substitute 0 (Var 1) (Var 2)) (Var 2) "2[0 := 1] leaves 2 unchanged"

  assertEq "Core::TestNormal" (substitute 0 (Var 1) (Lam 0 (Var 0) (Var 0))) (Lam 0 (Var 1) (Var 0)) "(Lam 0 : 0 . 0)[0 := 1] renames free type context to Lam 0 : 1 . 0"
  assertEq "Core::TestNormal" (substitute 0 (Var 1) (Lam 2 (Var 0) (Var 0))) (Lam 2 (Var 1) (Var 1)) "(Lam 2 : 0 . 0)[0 := 1] renames all to Lam 2 : 1 . 1"

  assertEq "Core::TestNormal" (substitute 0 (Var 1) (Pi 0 (Var 0) (Var 0))) (Pi 0 (Var 1) (Var 0)) "(Pi 0 : 0 . 0)[0 := 1] renames free type context to Pi 0 : 1 . 0"
  assertEq "Core::TestNormal" (substitute 0 (Var 1) (Pi 2 (Var 0) (Var 0))) (Pi 2 (Var 1) (Var 1)) "(Pi 2 : 0 . 0)[0 := 1] renames all to Pi 2 : 1 . 1"

  assertEq "Core::TestNormal" (substitute 0 (Var 1) (Var 0 `App` Var 0)) (Var 1 `App` Var 1) "(0 0)[0 := 1] renames to 1 1"

testNormalize :: IO ()
testNormalize = do
  let param a x e = Lam a Star (Lam x (Var a) e)
  assertEq "Core::TestNormal" (normalize (param 0 1 $ param 2 3 (Var 3) `App` Var 0 `App` Var 1)) (param 0 1 (Var 1)) "Id Id normalizes to Id"
