{-# LANGUAGE LambdaCase #-}

module L3.Parse.TestParser (tests) where

import L3.Core
import L3.Parse
import L3.Util
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
  assertRight "Parse::TestParser" (parseT "*") "Parse star * atom"
  assertRight "Parse::TestParser" (parseT "#") "Parse box # atom"

  assertRight "Parse::TestParser" (parseT "x") "Parse variable x atom"
  assertRight "Parse::TestParser" (parseT "x@y") "Parse namespaced variable x@y atom"

  assertRight "Parse::TestParser" (parseT "λ (a : *) -> a") "Parse lambda λ(a:*) -> a expression"
  assertRight "Parse::TestParser" (parseT "π (a : *) -> a") "Parse pi π(a:*) -> a expression"
  assertRight "Parse::TestParser" (parseT "π (a : *) -> a -> a") "Parse anonymous pi π(a:*) -> a -> a expression"

  assertRight "Parse::TestParser" (parseT "f x") "Parse application f x expression"

  assertRight "Parse::TestParser" (parseT "λ (a : m b -> c) -> d") "Parse monadic anonymous (applicative) pi expression"
