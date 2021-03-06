module L3.Core.TestInfer (tests) where

import L3.Core
import L3.Log
import L3.Util
import Test

tests :: [IO ()]
tests =
  [ testInferType,
    testInferTypeError,
    testWellTyped,
    testWellTypedError
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

testInferTypeError :: IO ()
testInferTypeError = do
  assertEq "Core::TestInfer" (inferType0 $ (Box :: Expr Name)) (Left $ Error (["in context:"], Just (Error (["absurd box"], Nothing)))) "# :: !! absurd"

  assertEq "Core::TestInfer" (inferType0 $ Var $ Name "x") (Left $ Error (["in context:"], Just (Error (["unbound variable:", "| x"], Nothing)))) "x :: !! unbound"

  assertEq "Core::TestInfer" (inferType0 $ Pi (Name "x") (Lam (Name "y") Star (Var $ Name "y")) Star) (Left $ Error (["in context:"], Just (Error (["invalid type:", "| π [x : λ [y : *] -> y] -> *", "had left kind:", "| π [y : *] -> *", "had right kind:", "| #"], Nothing)))) "pi x : (lam y : * -> y) -> * :: !! invalid-kind"
  assertEq "Core::TestInfer" (inferType0 $ Pi (Name "x") Star (Lam (Name "y") Star (Var $ Name "y"))) (Left $ Error (["in context:"], Just (Error (["invalid type:", "| π [x : *] -> λ [y : *] -> y", "had left kind:", "| #", "had right kind:", "| π [y : *] -> *"], Nothing)))) "pi x : * -> (lam y : * -> y) :: !! invalid-kind"

  assertEq "Core::TestInfer" (inferType0 $ Lam (Name "x") Star ((Var $ Name "x") `App` (Var $ Name "x"))) (Left $ Error (["in context:", "| (x,*)"], Just (Error (["cannot apply to non-function:", "| x", "had type: ", "| *", "had application:", "| x"], Nothing)))) "lam x : * -> x x :: !! non-function"

  assertEq "Core::TestInfer" (inferType0 $ Lam (Name "x") Star (Var $ Name "x") `App` Star) (Left $ Error (["in context:"],Just (Error (["type mismatch for function:","| λ [x : *] -> x","expected type:","| *","but given arg:","| *","and given type:","| #"],Nothing)))) "(lam x : * -> x) * :: !! mismatch"

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

testWellTypedError :: IO ()
testWellTypedError = do
  assertFalse "Core::TestInfer" (wellTyped0 $ (Box :: Expr Name)) "# :: !! absurd"

  assertFalse "Core::TestInfer" (wellTyped0 $ Var $ Name "x") "x :: !! unbound"

  assertFalse "Core::TestInfer" (wellTyped0 $ Pi (Name "x") (Lam (Name "y") Star (Var $ Name "y")) Star) "pi x : (lam y : * -> y) -> * :: !! invalid-kind"
  assertFalse "Core::TestInfer" (wellTyped0 $ Pi (Name "x") Star (Lam (Name "y") Star (Var $ Name "y"))) "pi x : * -> (lam y : * -> y) :: !! invalid-kind"

  assertFalse "Core::TestInfer" (wellTyped0 $ Lam (Name "x") Star ((Var $ Name "x") `App` (Var $ Name "x"))) "lam x : * -> x x :: !! non-function"

  assertFalse "Core::TestInfer" (wellTyped0 $ Lam (Name "x") Star (Var $ Name "x") `App` Star) "(lam x : * -> x) * :: !! mismatch"
