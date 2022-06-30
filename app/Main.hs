-- | Parsing for REPL commands
module Main where

import Data.Foldable (forM_)
import L3.Core
import L3.Loader
import L3.Log
import L3.Parse
import L3.Util
import System.Console.Haskeline

-- | Run REPL
main :: IO ()
main = withStdoutLogging $ do
  setLogLevel LevelInfo
  let (τ, prel) = wrapPrelude embeddedPrelude
  runInputT defaultSettings $ repl τ prel

repl :: ShowCtx -> (ShowExpr -> ShowExpr) -> InputT IO ()
repl τ prel = do
  isTerminalUI <- haveTerminalUI
  input' <- getInputLine (if isTerminalUI then "λ>> " else "")
  forM_ input' parse
  where
    parse :: String -> InputT IO ()
    parse inp = do
      let prelExpr = mapR prel $ fmapR parseExpr $ lexSrc inp
      case fmapR (evalExpr τ) prelExpr of
        Left err -> outputStrLn $ show err
        Right (t, e) -> do
          outputStrLn $ showExpr t
          outputStrLn $ showExpr $ normalize0 e
      repl τ prel
