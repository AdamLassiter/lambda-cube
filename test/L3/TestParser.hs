{-# LANGUAGE LambdaCase #-}

module L3.TestParser (tests) where

import L3.Parser
import Test

tests :: [IO ()]
tests =
  [ testParser
  ]

parseT :: String -> Result ShowExpr
parseT = fmapR parseExpr . lexSrc

assertRight :: String -> Result ShowExpr -> String -> IO ()
assertRight src x = assertTrue src pred
  where
    pred = case x of
      Right _ -> True
      Left _ -> False

testParser :: IO ()
testParser = do
  assertRight "TestParser" (parseT "*") "Parse star * atom"
  assertRight "TestParser" (parseT "#") "Parse box # atom"

  assertRight "TestParser" (parseT "x") "Parse variable x atom"
  assertRight "TestParser" (parseT "x@y") "Parse namespaced variable x@y atom"

  assertRight "TestParser" (parseT "λ (a : *) -> a") "Parse lambda λ(a:*) -> a expression"
  assertRight "TestParser" (parseT "π (a : *) -> a") "Parse pi π(a:*) -> a expression"
  assertRight "TestParser" (parseT "π (a : *) -> a -> a") "Parse anonymous pi π(a:*) -> a -> a expression"

  assertRight "TestParser" (parseT "f x") "Parse application f x expression"

  assertRight "TestParser" (parseT "lambda (a : m b -> c) -> d") "Parse monadic anonymous (applicative) pi expression"
