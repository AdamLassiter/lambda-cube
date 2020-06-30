module Main where

  import qualified LambdaPi
  import qualified SimplyTyped

  main :: IO ()
  main = do
    SimplyTyped.main
    LambdaPi.main
