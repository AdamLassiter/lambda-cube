module Main where
  import qualified Repl
  import qualified Test

  main :: IO ()
  main = do
    putStrLn $ "=================="
    putStrLn $ "Running Test Suite"
    putStrLn $ "=================="
    Test.test

    putStrLn $ "=================="
    putStrLn $ "Constructions REPL"
    putStrLn $ "=================="
    Repl.main
