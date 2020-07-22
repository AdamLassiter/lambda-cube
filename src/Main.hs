module Main where

  import qualified Repl
  import qualified Test

  main :: IO ()
  main = do
    putStrLn $ "=================="
    putStrLn $ "Running Test Suite"
    putStrLn $ "=================="
    putStrLn ""

    Test.test
    putStrLn ""

    putStrLn $ "=================="
    putStrLn $ "Constructions REPL"
    putStrLn $ "=================="
    putStrLn ""

    Repl.main
