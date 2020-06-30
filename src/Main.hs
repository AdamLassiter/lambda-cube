module Main where

  import qualified LambdaPi
  import qualified SimplyTyped

  main :: IO ()
  main = do
    putStrLn $ "==================="
    putStrLn $ "Running Test Suites"
    putStrLn $ "==================="
    putStrLn ""

    SimplyTyped.test
    putStrLn ""

    LambdaPi.test
    putStrLn ""
