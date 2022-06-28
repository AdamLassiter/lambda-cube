module L3.TestCore (tests) where

import L3.Core
import L3.Logging (warnM)
import Test

tests :: [IO ()]
tests =
  [ testShowCtx,
    testEnumInstance,
    testFree,
    testSubstitute,
    testNormalize,
    testIndex,
    testAlphaEq,
    testBetaEq,
    testInferType,
    testWellTyped
  ]

assertAlphaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertAlphaEq src = assert src (alphaEq, "=α=")

assertNotAlphaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertNotAlphaEq src = assert src ((not .) . alphaEq, "=α=")

assertBetaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertBetaEq src = assert src (betaEq, "=β=")

assertNotBetaEq :: (Show a, Eq a, Enum a) => String -> Expr a -> Expr a -> String -> IO ()
assertNotBetaEq src = assert src ((not .) . betaEq, "=β=")

testInstanceEnum :: IO ()
testInstanceEnum = do
  assertEq "TestCore" ((toEnum . fromEnum) (Left 1 :: Either Int Int)) (Left 1 :: Either Int Int) "Instance preserves invariant for left Enum"
  assertEq "TestCore" ((toEnum . fromEnum) (Right 1 :: Either Int Int)) (Right 1 :: Either Int Int) "Instance preserves invariant for right Enum"

testShowCtx :: IO ()
testShowCtx = do
  assertEq "TestCore" (showCtx (Ctx [] :: Context Int)) "" "Show empty context"
  assertEq "TestCore" (showCtx (Ctx [(1, Star)] :: Context Int)) "\n(1,*)" "Show one-elem context"

testEnumInstance :: IO ()
testEnumInstance = do
  assertEq "TestCore" ((toEnum . fromEnum) (Left 0 :: Either Int Int)) (Left 0 :: Either Int Int) "Left From<->To enum is id invariant"
  assertEq "TestCore" ((toEnum . fromEnum) (Left 1 :: Either Int Int)) (Left 1 :: Either Int Int) "Left From<->To enum is id invariant"
  assertEq "TestCore" ((toEnum . fromEnum) (Right 0 :: Either Int Int)) (Right 0 :: Either Int Int) "Right From<->To enum is id invariant"
  assertEq "TestCore" ((toEnum . fromEnum) (Right 1 :: Either Int Int)) (Right 1 :: Either Int Int) "Right From<->To enum is id invariant"

testFree :: IO ()
testFree = do
  assertFalse "TestCore" (free 0 Star) "0 is not free in Star"
  assertFalse "TestCore" (free 0 Box) "0 is not free in Box"

  assertTrue "TestCore" (free 0 (Var 0)) "0 is free in Var 0"
  assertFalse "TestCore" (free 0 (Var 1)) "0 is not free in Var 1"

  assertFalse "TestCore" (free 0 (Lam 0 (Var 1) (Var 1))) "0 is not free in Lam 0 : 1 . 1"
  assertTrue "TestCore" (free 0 (Lam 0 (Var 0) (Var 1))) "0 is free in Lam 0 : 0 . 1"
  assertFalse "TestCore" (free 0 (Lam 0 (Var 1) (Var 0))) "0 is not free in Lam 0 : 1 . 0"
  assertTrue "TestCore" (free 0 (Lam 1 (Var 0) Star)) "0 is free in Lam 1 : 0 . *"
  assertTrue "TestCore" (free 0 (Lam 1 Star (Var 0))) "0 is free in Lam 1 : 0 . *"
  assertFalse "TestCore" (free 0 (Lam 1 (Var 1) (Var 1))) "0 is not free in Lam 1 : 1 . 1"

  assertFalse "TestCore" (free 0 (Pi 0 (Var 1) (Var 1))) "0 is not free in Pi 0 : 1 . 1"
  assertTrue "TestCore" (free 0 (Pi 0 (Var 0) (Var 1))) "0 is free in Pi 0 : 0 . 1"
  assertFalse "TestCore" (free 0 (Pi 0 (Var 1) (Var 0))) "0 is not free in Pi 0 : 1 . 0"
  assertTrue "TestCore" (free 0 (Pi 1 (Var 0) Star)) "0 is free in Pi 1 : 0 . *"
  assertTrue "TestCore" (free 0 (Pi 1 Star (Var 0))) "0 is free in Pi 1 : 0 . *"
  assertFalse "TestCore" (free 0 (Pi 1 (Var 1) (Var 1))) "0 is not free in Pi 1 : 1 . 1"

  assertFalse "TestCore" (free 0 (Star `App` Star)) "0 is not free in * *"
  assertTrue "TestCore" (free 0 (Var 0 `App` Star)) "0 is free in 0 *"
  assertTrue "TestCore" (free 0 (Star `App` Var 0)) "0 is free in * 0"
  assertTrue "TestCore" (free 0 (Var 0 `App` Var 0)) "0 is free in 0 0"

testSubstitute :: IO ()
testSubstitute = do
  assertEq "TestCore" (substitute 0 (Var 1) Star) Star "*[0 := 1] leaves * unchanged"
  assertEq "TestCore" (substitute 0 (Var 1) Box) Box "#[0 := 1] leaves # unchanged"

  assertEq "TestCore" (substitute 0 (Var 1) (Var 0)) (Var 1) "0[0 := 1] renames to 1"
  assertEq "TestCore" (substitute 0 (Var 1) (Var 2)) (Var 2) "2[0 := 1] leaves 2 unchanged"

  assertEq "TestCore" (substitute 0 (Var 1) (Lam 0 (Var 0) (Var 0))) (Lam 0 (Var 1) (Var 0)) "(Lam 0 : 0 . 0)[0 := 1] renames free type context to Lam 0 : 1 . 0"
  assertEq "TestCore" (substitute 0 (Var 1) (Lam 2 (Var 0) (Var 0))) (Lam 2 (Var 1) (Var 1)) "(Lam 2 : 0 . 0)[0 := 1] renames all to Lam 2 : 1 . 1"

  assertEq "TestCore" (substitute 0 (Var 1) (Pi 0 (Var 0) (Var 0))) (Pi 0 (Var 1) (Var 0)) "(Pi 0 : 0 . 0)[0 := 1] renames free type context to Pi 0 : 1 . 0"
  assertEq "TestCore" (substitute 0 (Var 1) (Pi 2 (Var 0) (Var 0))) (Pi 2 (Var 1) (Var 1)) "(Pi 2 : 0 . 0)[0 := 1] renames all to Pi 2 : 1 . 1"

  assertEq "TestCore" (substitute 0 (Var 1) (Var 0 `App` Var 0)) (Var 1 `App` Var 1) "(0 0)[0 := 1] renames to 1 1"

testNormalize :: IO ()
testNormalize = do
  let param a x e = Lam a Star (Lam x (Var a) e)
  assertEq "TestCore" (normalize (param 0 1 $ param 2 3 (Var 3) `App` Var 0 `App` Var 1)) (param 0 1 (Var 1)) "Id Id normalizes to Id"

testIndex :: IO ()
testIndex = do
  assertEq "TestCore" (index0 (Star :: ShowExpr)) Star "Star indexes to star"
  assertEq "TestCore" (index0 (Box :: ShowExpr)) Box "Box indexes to box"

  assertEq "TestCore" (index0 (Var (Name "x"))) (Var $ Right (Name "x")) "Unbound names cannot be indexed"

  assertEq "TestCore" (index0 $ Lam (Name "x") Star (Var (Name "x"))) (Lam (Left 0) Star (Var $ Left 0)) "Bound names can be indexed"
  assertEq "TestCore" (index0 $ Lam (Name "x") (Var (Name "x")) (Var (Name "x"))) (Lam (Left 0) (Var $ Right (Name "x")) (Var $ Left 0)) "Types are unbound relative to their binding"

  assertEq "TestCore" (index0 $ Pi (Name "x") Star (Var (Name "x"))) (Pi (Left 0) Star (Var $ Left 0)) "Bound names can be indexed"
  assertEq "TestCore" (index0 $ Pi (Name "x") (Var (Name "x")) (Var (Name "x"))) (Pi (Left 0) (Var $ Right (Name "x")) (Var $ Left 0)) "Types are unbound relative to their binding"

  assertEq "TestCore" (index0 $ Var (Name "x") `App` Var (Name "y")) (Var (Right (Name "x")) `App` Var (Right (Name "y"))) "Unbound names cannot be indexed over applications"
  assertEq "TestCore" (index0 $ Lam (Name "x") Star (Var (Name "x")) `App` Lam (Name "y") Star (Var (Name "y"))) (Lam (Left 0) Star (Var $ Left 0) `App` Lam (Left 0) Star (Var $ Left 0)) "Bound names can be indexed over applications"

testAlphaEq :: IO ()
testAlphaEq = do
  assertAlphaEq "TestCore" (Var (Name "x")) (Var (Name "x")) "Matching unbound names are alpha-equivalent"
  assertNotAlphaEq "TestCore" (Var (Name "x")) (Var (Name "y")) "Non-matching unbound names are not alpha-equivalent"

  assertAlphaEq "TestCore" (Lam (Name "x") Star (Var (Name "x"))) (Lam (Name "y") Star (Var (Name "y"))) "Bound names are alpha-equivalent over lambdas"
  assertAlphaEq "TestCore" (Pi (Name "x") Star (Var (Name "x"))) (Pi (Name "y") Star (Var (Name "y"))) "Bound names are alpha-equivalent over pis"

  assertAlphaEq "TestCore" (Lam (Name "x") Star (Var (Name "x")) `App` Var (Name "y")) (Lam (Name "y") Star (Var (Name "y")) `App` Var (Name "y")) "Alpha-equivalence distributes over application"

testBetaEq :: IO ()
testBetaEq =
  assertBetaEq "TestCore" (Lam (Name "A") Star (Lam (Name "X") (Var (Name "A")) (Lam (Name "a") Star (Lam (Name "x") (Var (Name "a")) (Var (Name "x"))) `App` Var (Name "A") `App` Var (Name "X")))) (Lam (Name "a") Star (Lam (Name "x") (Var (Name "a")) (Var (Name "x")))) "Id Id is equivalent to Id"

testInferType :: IO ()
testInferType = do
  assertEq "TestCore" (inferType0 (Star :: Expr Name)) (Right Box) "* :: #"

  assertEq "TestCore" (inferType (Ctx [(Name "x", Star)]) (Var (Name "x"))) (Right Star) "x :: * ⊢ x :: *"

  assertEq "TestCore" (inferType0 (Lam (Name "x") Star (Var (Name "x")))) (Right $ Pi (Name "x") Star Star) "lam x : * -> x :: pi x : * -> *"
  assertEq "TestCore" (inferType0 (Lam (Name "x") Star (Var (Name "x")))) (Right $ Pi (Name "x") Star Star) "lam x : * -> x :: pi x : * -> *"

  assertEq "TestCore" (inferType (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) (Var (Name "a")))) (Right Star) "a :: * ⊢ pi x : a -> a :: *"
  assertEq "TestCore" (inferType0 (Pi (Name "x") Star (Var (Name "x")))) (Right Star) "pi x : * -> x :: *"
  assertEq "TestCore" (inferType (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) Star)) (Right Box) "a :: * ⊢ pi x : * -> * :: #"
  assertEq "TestCore" (inferType0 (Pi (Name "x") Star Star)) (Right Box) "pi x : * -> * :: #"

  let ctx =
        [ (Name "f", Pi (Name "x") (Var $ Name "b") (Var $ Name "c")),
          (Name "c", Star),
          (Name "b", Star),
          (Name "map", Pi (Name "a") Star (Pi (Name "b") Star (Pi (Name "f") (Pi (Name "x") (Var $ Name "a") (Var $ Name "b")) (Pi (Name "l") (App (Var $ Name "List") (Var $ Name "a")) (App (Var $ Name "List") (Var $ Name "b")))))),
          (Name "List", Pi (Name "a") Star Star)
        ]
  assertEq "TestCore" (inferType (Ctx ctx) (Var (Name "map") `App` Var (Name "b") `App` Var (Name "c") `App` Var (Name "f"))) (Right $ Pi (Name "l") (App (Var $ Name "List") (Var $ Name "b")) (App (Var $ Name "List") (Var $ Name "c"))) "map :: pi a -> pi b -> lam (a -> b) -> List a -> List b, f :: b -> c ⊢ map b c f :: List b -> List c"

testWellTyped :: IO ()
testWellTyped = do
  assertTrue "TestCore" (wellTyped0 (Star :: Expr Name)) "* :: #"

  assertTrue "TestCore" (wellTyped (Ctx [(Name "x", Star)]) (Var (Name "x"))) "x :: * ⊢ x :: *"

  assertTrue "TestCore" (wellTyped0 (Lam (Name "x") Star (Var (Name "x")))) "lam x : * -> x :: pi x : * -> *"
  assertTrue "TestCore" (wellTyped0 (Lam (Name "x") Star (Var (Name "x")))) "lam x : * -> x :: pi x : * -> *"

  assertTrue "TestCore" (wellTyped (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) (Var (Name "a")))) "a :: * ⊢ pi x : a -> a :: *"
  assertTrue "TestCore" (wellTyped0 (Pi (Name "x") Star (Var (Name "x")))) "pi x : * -> x :: *"
  assertTrue "TestCore" (wellTyped (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) Star)) "a :: * ⊢ pi x : * -> * :: #"
  assertTrue "TestCore" (wellTyped0 (Pi (Name "x") Star Star)) "pi x : * -> * :: #"

  let ctx =
        [ (Name "f", Pi (Name "x") (Var $ Name "b") (Var $ Name "c")),
          (Name "c", Star),
          (Name "b", Star),
          (Name "map", Pi (Name "a") Star (Pi (Name "b") Star (Pi (Name "f") (Pi (Name "x") (Var $ Name "a") (Var $ Name "b")) (Pi (Name "l") (App (Var $ Name "List") (Var $ Name "a")) (App (Var $ Name "List") (Var $ Name "b")))))),
          (Name "List", Pi (Name "a") Star Star)
        ]
  assertTrue "TestCore" (wellTyped (Ctx ctx) (Var (Name "map") `App` Var (Name "b") `App` Var (Name "c") `App` Var (Name "f"))) "map :: pi a -> pi b -> lam (a -> b) -> List a -> List b, f :: b -> c ⊢ map b c f :: List b -> List c"
