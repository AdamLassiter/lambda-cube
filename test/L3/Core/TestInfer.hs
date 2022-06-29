module L3.Core.TestInfer (tests) where

import L3.Core
import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testInferType,
    testWellTyped
  ]

testInferType :: IO ()
testInferType = do
  assertEq "Core::TestInfer" (inferType0 (Star :: Expr Name)) (Right Box) "* :: #"

  assertEq "Core::TestInfer" (inferType (Ctx [(Name "x", Star)]) (Var (Name "x"))) (Right Star) "x :: * ⊢ x :: *"

  assertEq "Core::TestInfer" (inferType0 (Lam (Name "x") Star (Var (Name "x")))) (Right $ Pi (Name "x") Star Star) "lam x : * -> x :: pi x : * -> *"
  assertEq "Core::TestInfer" (inferType0 (Lam (Name "x") Star (Var (Name "x")))) (Right $ Pi (Name "x") Star Star) "lam x : * -> x :: pi x : * -> *"

  assertEq "Core::TestInfer" (inferType (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) (Var (Name "a")))) (Right Star) "a :: * ⊢ pi x : a -> a :: *"
  assertEq "Core::TestInfer" (inferType0 (Pi (Name "x") Star (Var (Name "x")))) (Right Star) "pi x : * -> x :: *"
  assertEq "Core::TestInfer" (inferType (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) Star)) (Right Box) "a :: * ⊢ pi x : * -> * :: #"
  assertEq "Core::TestInfer" (inferType0 (Pi (Name "x") Star Star)) (Right Box) "pi x : * -> * :: #"

  let ctx =
        [ (Name "f", Pi (Name "x") (Var $ Name "b") (Var $ Name "c")),
          (Name "c", Star),
          (Name "b", Star),
          (Name "map", Pi (Name "a") Star (Pi (Name "b") Star (Pi (Name "f") (Pi (Name "x") (Var $ Name "a") (Var $ Name "b")) (Pi (Name "l") (App (Var $ Name "List") (Var $ Name "a")) (App (Var $ Name "List") (Var $ Name "b")))))),
          (Name "List", Pi (Name "a") Star Star)
        ]
  assertEq "Core::TestInfer" (inferType (Ctx ctx) (Var (Name "map") `App` Var (Name "b") `App` Var (Name "c") `App` Var (Name "f"))) (Right $ Pi (Name "l") (App (Var $ Name "List") (Var $ Name "b")) (App (Var $ Name "List") (Var $ Name "c"))) "map :: pi a -> pi b -> lam (a -> b) -> List a -> List b, f :: b -> c ⊢ map b c f :: List b -> List c"

testWellTyped :: IO ()
testWellTyped = do
  assertTrue "Core::TestInfer" (wellTyped0 (Star :: Expr Name)) "* :: #"

  assertTrue "Core::TestInfer" (wellTyped (Ctx [(Name "x", Star)]) (Var (Name "x"))) "x :: * ⊢ x :: *"

  assertTrue "Core::TestInfer" (wellTyped0 (Lam (Name "x") Star (Var (Name "x")))) "lam x : * -> x :: pi x : * -> *"
  assertTrue "Core::TestInfer" (wellTyped0 (Lam (Name "x") Star (Var (Name "x")))) "lam x : * -> x :: pi x : * -> *"

  assertTrue "Core::TestInfer" (wellTyped (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) (Var (Name "a")))) "a :: * ⊢ pi x : a -> a :: *"
  assertTrue "Core::TestInfer" (wellTyped0 (Pi (Name "x") Star (Var (Name "x")))) "pi x : * -> x :: *"
  assertTrue "Core::TestInfer" (wellTyped (Ctx [(Name "a", Star)]) (Pi (Name "x") (Var (Name "a")) Star)) "a :: * ⊢ pi x : * -> * :: #"
  assertTrue "Core::TestInfer" (wellTyped0 (Pi (Name "x") Star Star)) "pi x : * -> * :: #"

  let ctx =
        [ (Name "f", Pi (Name "x") (Var $ Name "b") (Var $ Name "c")),
          (Name "c", Star),
          (Name "b", Star),
          (Name "map", Pi (Name "a") Star (Pi (Name "b") Star (Pi (Name "f") (Pi (Name "x") (Var $ Name "a") (Var $ Name "b")) (Pi (Name "l") (App (Var $ Name "List") (Var $ Name "a")) (App (Var $ Name "List") (Var $ Name "b")))))),
          (Name "List", Pi (Name "a") Star Star)
        ]
  assertTrue "Core::TestInfer" (wellTyped (Ctx ctx) (Var (Name "map") `App` Var (Name "b") `App` Var (Name "c") `App` Var (Name "f"))) "map :: pi a -> pi b -> lam (a -> b) -> List a -> List b, f :: b -> c ⊢ map b c f :: List b -> List c"
