module Util where

    -- result types and error throwing

    type Result a = Either String a

    throwError :: String -> Result a
    throwError = error


    -- poor man's unittest lib --

    assertEquals :: (Show a, Eq a) => a -> a -> a
    assertEquals x y = case x == y of
        True  -> x
        False -> error $ (show x) ++ " != " ++ (show y)