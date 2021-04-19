-- Parsing for REPL commands
module Main where
    import L3.Pretty
    import L3.Parser
    import L3.Util

    import Control.Applicative hiding (some, many)
    import System.Console.Haskeline


    -- Run REPL
    main :: IO ()
    main = runInputT defaultSettings repl

    repl :: InputT IO ()
    repl = do
        isTerminalUI <- haveTerminalUI
        input' <- getInputLine (if isTerminalUI then ">> " else "")
        result <- case input' of
            Just inp -> parse inp
            Nothing -> return ()
        return $ result
        where parse :: String -> InputT IO ()
              parse inp = do
                  let (typ, expr) = eval inp
                  case typ of
                      Left err -> outputStrLn $ err
                      Right t -> outputStrLn $ showExpr id t
                  case expr of
                      Left err -> outputStrLn $ err
                      Right e -> outputStrLn $ showExpr id e
                  repl