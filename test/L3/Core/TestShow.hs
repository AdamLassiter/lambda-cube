module L3.Core.TestShow (tests) where

import L3.Core
import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testShowCtx
  ]

testShowCtx :: IO ()
testShowCtx = do
  assertEq "Core::TestShow" (showCtx (Ctx [] :: Context Int)) "" "Show empty context"
  assertEq "Core::TestShow" (showCtx (Ctx [(1, Star)] :: Context Int)) "\n(1,*)" "Show one-elem context"
