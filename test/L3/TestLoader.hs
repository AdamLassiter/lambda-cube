module L3.TestLoader (tests) where

import L3.Loader
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

showSome :: (Show a, Show b) => [(a, b)] -> String
showSome [] = "[  ]"
showSome x = "[" ++ show (fst $ head x) ++ " .. [+" ++ show (length x - 2) ++ " elements] .. " ++ show (fst $ last x) ++ "]"

assertListEq :: (Eq a, Show a, Eq b, Show b) => String -> [(a, b)] -> [(a, b)] -> String -> IO ()
assertListEq src = assertShowing src showSome ((==), "==")

assertListNotEmpty :: (Eq a, Show a, Eq b, Show b) => String -> [(a, b)] -> String -> IO ()
assertListNotEmpty src x = assertShowing src showSome ((/=), "/=") x []

assertBetaEq :: String -> ShowExpr -> ShowExpr -> String -> IO ()
assertBetaEq src = assertShowing src (showExpr . normalize0) (betaEq, "=β=")

testTakeDirectoryName :: IO ()
testTakeDirectoryName = do
  let fp = "root" </> "directory" </> "file" <.> "ext"
  assertEq "TestLoader" (takeDirectoryName fp) "directory" "Take directory from filepath"

testTakeNamespacedFileName :: IO ()
testTakeNamespacedFileName = do
  let noNs = "." </> "file" <.> "ext"
  assertEq "TestLoader" (takeNamespacedFileName noNs) "file" "Take filename when not namespaced"
  let ns = "Bool" </> "file" <.> "ext"
  assertEq "TestLoader" (takeNamespacedFileName ns) "Bool@file" "Take dir@filename when namespaced"

testEmbeddedPrelude :: IO ()
testEmbeddedPrelude = do
  ioPrelude <- embeddedPreludeIO
  assertListNotEmpty "TestLoader" ioPrelude "IO Prelude is non-empty"
  assertListNotEmpty "TestLoader" embeddedPrelude "Prelude is non-empty"
  assertListEq "TestLoader" embeddedPrelude ioPrelude "Prelude is IO Prelude"

testLoadPrelude :: IO ()
testLoadPrelude = do
  let (Ctx τCtx, Ctx ε) = loadPrelude embeddedPrelude
  assertListNotEmpty "TestLoader" τCtx "Type-context is non-empty"
  assertListNotEmpty "TestLoader" ε "Expression-context is non-empty"

testTauSubst :: IO ()
testTauSubst = do
  assertEq "TestLoader" (tauSubst (Name "x") (Var (Name "y")) $ Lam (Name "x") (Var $ Name "T") $ Var $ Name "x") (Lam (Name "x") (Var $ Name "T") $ Var $ Name "x") "(lam x : T -> x)[τ/x := y] renames [nothing]"
  assertEq "TestLoader" (tauSubst (Name "T") (Var (Name "y")) $ Lam (Name "x") (Var $ Name "T") $ Var $ Name "x") (Lam (Name "x") (Var $ Name "y") $ Var $ Name "x") "(lam x : T -> x)[τ/T := y] renames T"

testTauNorm :: IO ()
testTauNorm = do
  assertEq "TestLoader" (tauNorm (Star :: Expr Name)) Star "* =γ= *"

  let boolT = Pi (Name "Bool") Star $ Pi (Name "True") (Var $ Name "Bool") $ Pi (Name "False") (Var $ Name "Bool") $ Var $ Name "Bool"
  let boolF val = Lam (Name "Bool") Star $ Lam (Name "True") (Var $ Name "Bool") $ Lam (Name "False") (Var $ Name "Bool") val
  let not = Lam (Name "x") boolT $ (Var $ Name "x") `App` boolT `App` (boolF $ Var $ Name "False") `App` (boolF $ Var $ Name "True")
  let notT = throwL $ inferType0 not
  let bind expr = ((Lam (Name "Bool") Star $ (Lam (Name "not") notT $ expr) `App` not) `App` boolT)
  assertEq "TestLoader" (tauNorm $ bind $ Lam (Name "x") (Var $ Name "Bool") ((Var $ Name "not") `App` (Var $ Name "x"))) (bind $ Lam (Name "x") boolT ((Var $ Name "not") `App` (Var $ Name "x"))) "bind [Bool, not] ⊢ (lam x : Bool -> not x) =γ= (lam x : T[T := Bool] -> not x)"

testWrapPrelude :: IO ()
testWrapPrelude = do
  let (τCtx, prelWrap) = wrapPrelude embeddedPrelude
  assertBetaEq "TestLoader" (prelWrap $ throwL $ parseExpr $ throwL $ lexSrc "id") (throwL $ parseExpr $ throwL $ lexSrc "λ(a:*) -> λ(x:a) -> x") "Prelude@id =α= id"
