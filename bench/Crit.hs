module Main where

import Criterion.Main
import L3.Core
import L3.Loader
import L3.Log
import L3.Parse
import L3.Util

-- | Run benchmark
main :: IO ()
main =
  withStderrLogging $ do
    setLogLevel LevelInfo
    defaultMain
      [ benchEval "λ(T:*) . λ(x:T) . x",
        benchEval "λ(x:Nat) . Bool@eq (even x) (odd (Nat@Succ x))"
      ]

benchEval :: String -> Benchmark
benchEval input = bench input $ whnf (evalExpr τ) prelExpr
  where
    (τ, prel) = wrapPrelude embeddedPrelude
    prelExpr = throwL $ mapR prel $ fmapR parseExpr $ lexSrc input