-- Parsing for REPL commands
module Main where
    import L3.Pretty (showExpr, ShowCtx, ShowExpr, evalExpr1, normalize0, fmapR, mapR)
    import L3.Parser (parseExpr)
    import L3.Loader (wrapPrelude)

    import Control.Applicative hiding (some, many)
    import Data.Foldable (forM_)

    import System.Console.Haskeline


    -- Run REPL
    main :: IO ()
    main = do
      (tCtx, prel) <- wrapPrelude
      runInputT defaultSettings $ repl tCtx prel

    repl :: ShowCtx -> (ShowExpr -> ShowExpr) -> InputT IO ()
    repl tCtx prel = do
        isTerminalUI <- haveTerminalUI
        input' <- getInputLine (if isTerminalUI then ">> " else "")
        forM_ input' parse
        where parse :: String -> InputT IO ()
              parse inp = do
                  let prEx = mapR prel $ parseExpr inp
                  case fmapR (evalExpr1 tCtx) prEx of
                      Left err -> outputStrLn err
                      Right (t, e) -> do
                          outputStrLn $ showExpr t
                          outputStrLn $ showExpr $ normalize0 e
                  repl tCtx prel
