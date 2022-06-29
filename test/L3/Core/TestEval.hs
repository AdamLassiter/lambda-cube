module L3.Core.TestEval (tests) where

import L3.Core
import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testEval
  ]

testEval :: IO ()
testEval = do
  let τCtx = Ctx [(Name "t", Star), (Name "x", Var $ Name "t")] 
  let id = Lam (Name "T") Star $ Lam (Name "X") (Var $ Name "T") $ (Var $ Name "X")
  assertEq "Core::TestEval" (evalExpr τCtx $ id `App` (Var $ Name "t") `App` (Var $ Name "x")) (Right (Var $ Name "t", Var $ Name "x")) "x :: t ⊢ id t x == x :: t"
