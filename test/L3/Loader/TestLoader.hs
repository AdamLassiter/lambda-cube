module L3.Loader.TestLoader (tests) where

import L3.Core
import L3.Loader
import L3.Parse
import L3.Util
import System.FilePath
import Test

tests :: [IO ()]
tests =
  [ testTakeDirectoryName,
    testTakeNamespacedFileName,
    testEmbeddedPrelude,
    testLoadPrelude,
    testTauSubst,
    testTauNorm,
    testWrapPrelude
  ]

testTakeDirectoryName :: IO ()
testTakeDirectoryName = do
  let fp = "root" </> "directory" </> "file" <.> "ext"
  assertEq "Loader::TestLoader" (takeDirectoryName fp) "directory" "Take directory from filepath"

testTakeNamespacedFileName :: IO ()
testTakeNamespacedFileName = do
  let noNs = "." </> "file" <.> "ext"
  assertEq "Loader::TestLoader" (takeNamespacedFileName noNs) "file" "Take filename when not namespaced"
  let ns = "Bool" </> "file" <.> "ext"
  assertEq "Loader::TestLoader" (takeNamespacedFileName ns) "Bool@file" "Take dir@filename when namespaced"

testEmbeddedPrelude :: IO ()
testEmbeddedPrelude = do
  prelude <- embeddedPreludeIO
  assertListNotEmpty "Loader::TestLoader" prelude "IO Prelude is non-empty"
  assertListNotEmpty "Loader::TestLoader" embeddedPrelude "Prelude is non-empty"
  assertListEq "Loader::TestLoader" embeddedPrelude prelude "Prelude is IO Prelude"

testLoadPrelude :: IO ()
testLoadPrelude = do
  let (Ctx τCtx, Ctx ε) = loadPrelude embeddedPrelude
  assertListNotEmpty "Loader::TestLoader" τCtx "Type-context is non-empty"
  assertListNotEmpty "Loader::TestLoader" ε "Expression-context is non-empty"

testTauSubst :: IO ()
testTauSubst = do
  assertEq "Loader::TestLoader" (tauSubst (Name "x") (Var (Name "y")) $ Lam (Name "x") (Var $ Name "T") $ Var $ Name "x") (Lam (Name "x") (Var $ Name "T") $ Var $ Name "x") "(lam x : T -> x)[τ/x := y] renames [nothing]"
  assertEq "Loader::TestLoader" (tauSubst (Name "T") (Var (Name "y")) $ Lam (Name "x") (Var $ Name "T") $ Var $ Name "x") (Lam (Name "x") (Var $ Name "y") $ Var $ Name "x") "(lam x : T -> x)[τ/T := y] renames T"

testTauNorm :: IO ()
testTauNorm = do
  assertEq "Loader::TestLoader" (tauNorm (Star :: Expr Name)) Star "* =γ= *"

  let boolT = Pi (Name "Bool") Star $ Pi (Name "True") (Var $ Name "Bool") $ Pi (Name "False") (Var $ Name "Bool") $ Var $ Name "Bool"
  let boolF val = Lam (Name "Bool") Star $ Lam (Name "True") (Var $ Name "Bool") $ Lam (Name "False") (Var $ Name "Bool") val
  let not = Lam (Name "x") boolT $ (Var $ Name "x") `App` boolT `App` (boolF $ Var $ Name "False") `App` (boolF $ Var $ Name "True")
  let notT = throwL $ inferType0 not
  let bind expr = ((Lam (Name "Bool") Star $ (Lam (Name "not") notT $ expr) `App` not) `App` boolT)
  assertEq "Loader::TestLoader" (tauNorm $ bind $ Lam (Name "x") (Var $ Name "Bool") ((Var $ Name "not") `App` (Var $ Name "x"))) (bind $ Lam (Name "x") boolT ((Var $ Name "not") `App` (Var $ Name "x"))) "bind [Bool, not] ⊢ (lam x : Bool -> not x) =γ= (lam x : T[T := Bool] -> not x)"

testWrapPrelude :: IO ()
testWrapPrelude = do
  let (τCtx, prelWrap) = wrapPrelude embeddedPrelude
  assertBetaEq "Loader::TestLoader" (prelWrap $ throwL $ parseExpr $ throwL $ lexSrc "id") (throwL $ parseExpr $ throwL $ lexSrc "λ(a:*) -> λ(x:a) -> x") "Prelude@id =α= id"
