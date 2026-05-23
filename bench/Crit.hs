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
    closedTerms <- mapM loadFileTerm closedExampleTerms
    setLogLevel LevelInfo
    defaultMain
      [ bgroup "small" $ map benchPreludeTerm smallTerms,
        bgroup "closed examples" $ map benchClosedTerm closedTerms
      ]

type Term = (String, String)

type FileTerm = (String, FilePath)

type ParsedTerm = (String, ShowExpr)

smallTerms :: [Term]
smallTerms =
  [ ("polymorphic identity", "λ(T:*) . λ(x:T) . x"),
    ("nat parity relation", "λ(x:Nat) . Bool@eq (even x) (odd (Nat@Succ x))")
  ]

closedExampleTerms :: [FileTerm]
closedExampleTerms =
  [ ("church list foldr/all", "test/Morte/example5.l3"),
    ("list map composition", "test/Morte/example9.l3"),
    ("stream map fusion", "test/Morte/example13.l3"),
    ("io replicate via church nat", "test/Morte/example14.l3")
  ]

benchPreludeTerm :: Term -> Benchmark
benchPreludeTerm (label, input) = bench label $ whnf (forceEvalResult . evalExpr τ) prelExpr
  where
    (τ, prel) = wrapPrelude embeddedPrelude
    prelExpr = throwL $ mapR prel $ fmapR parseExpr $ lexSrc input

loadFileTerm :: FileTerm -> IO ParsedTerm
loadFileTerm (label, path) = do
  expr <- parseFile path
  pure (label, expr)

benchClosedTerm :: ParsedTerm -> Benchmark
benchClosedTerm (label, expr) = bench label $ whnf (forceEvalResult . evalExpr (Ctx [])) expr

parseFile :: FilePath -> IO ShowExpr
parseFile path = do
  input <- readFile path
  let expr = parseTerm input
  expr `seq` pure expr

parseTerm :: String -> ShowExpr
parseTerm input = throwL $ fmapR parseExpr $ lexSrc input

forceEvalResult :: Result (Expr a, Expr a) -> ()
forceEvalResult (Left err) = error $ show err
forceEvalResult (Right (typ, expr)) = forceExpr typ `seq` forceExpr expr

forceExpr :: Expr a -> ()
forceExpr Star = ()
forceExpr Box = ()
forceExpr (Var v) = v `seq` ()
forceExpr (Lam v typ body) = v `seq` forceExpr typ `seq` forceExpr body
forceExpr (Pi v typ body) = v `seq` forceExpr typ `seq` forceExpr body
forceExpr (App f arg) = forceExpr f `seq` forceExpr arg
