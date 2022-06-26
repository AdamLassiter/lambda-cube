-- | Parsing for REPL commands
module Main where

import Data.Foldable (forM_)
import L3.Loader
import L3.Logging
import System.Console.Haskeline

-- | Run REPL
main :: IO ()
main = withStdoutLogging $ do
  let (tCtx, prel) = wrapPrelude embeddedPrelude
  -- let (tCtx, prel) = ([], id)
  runInputT defaultSettings $ repl tCtx prel

repl :: ShowCtx -> (ShowExpr -> ShowExpr) -> InputT IO ()
repl tCtx prel = do
  isTerminalUI <- haveTerminalUI
  input' <- getInputLine (if isTerminalUI then ">> " else "")
  forM_ input' parse
  where
    parse :: String -> InputT IO ()
    parse inp = do
      let prEx = mapR prel $ fmapR parseExpr $ lexSrc inp
      case fmapR (evalExpr tCtx) prEx of
        Left err -> outputStrLn $ show err
        Right (t, e) -> do
          outputStrLn $ showExpr t
          outputStrLn $ showExpr $ normalize0 e
      repl tCtx prel
