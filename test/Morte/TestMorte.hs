module Morte.TestMorte (tests) where

import L3.Loader
import L3.Logging
import Test

debugTestMorte = debugM "TestMorte"

errorTestMorte = errorU "TestMorte"

tests :: [IO ()]
tests =
  [ example0,
    example1,
    example2,
    example3,
    example4,
    example5,
    example6,
    example7,
    example8,
    example9,
    example10,
    example11,
    example12,
    example13,
    example14,
    example15
  ]

rstrip :: String -> String
rstrip = reverse . dropWhile (== '\n') . reverse

parse :: ShowCtx -> String -> (ShowExpr, ShowExpr)
parse τ inp = case fmapR (evalExpr τ) prEx of
  Left err -> errorTestMorte $ show err
  Right (t, e) -> (t, normalize0 e)
  where
    prEx = fmapR parseExpr $ lexSrc inp

example :: FilePath -> String -> String -> IO ()
example file typ expr = do
  contents <- readFile file
  let (typ', expr') = parse (Ctx []) expr
  let (typ0, expr0) = parse (Ctx []) contents
  debugTestMorte $ "== " ++ show file ++ " =="
  debugTestMorte typ
  debugTestMorte expr
  debugTestMorte $ "== Expression ==" ++ "\n" ++ rstrip contents
  debugTestMorte "== Type =="
  debugTestMorte $ "Expected: " ++ showExpr typ'
  debugTestMorte $ "Actual: " ++ showExpr typ0
  assertShowing "TestMorte" showExpr (alphaEq, "=α=") typ' typ0 "Expected type is alpha-equivalent to inferred actual type"
  debugTestMorte "== Normalization =="
  debugTestMorte $ "Expected: " ++ showExpr expr'
  debugTestMorte $ "Actual: " ++ showExpr expr0
  assertShowing "TestMorte" showExpr (alphaEq, "=α=") expr' expr0 "Expected expression is alpha-equivalent to normalized actual expression"
  return ()

example0 :: IO ()
example0 =
  example
    "test/Morte/example0.l3"
    "π (a : *) . π (x : a) . a"
    "λ (a : *) . λ (x : a) . x"

example1 :: IO ()
example1 =
  example
    "test/Morte/example1.l3"
    "π (String : *) . π (x : String) . String"
    "λ (String : *) . λ (x : String) . x"

example2 :: IO ()
example2 =
  example
    "test/Morte/example2.l3"
    "π (a : *) . a . a"
    "λ (a : *) . λ (x : a) . x"

example3 :: IO ()
example3 =
  example
    "test/Morte/example3.l3"
    "π (Int : *) . π (Zero : Int) . π (One : Int) . Int"
    "λ (Int : *) . λ (Zero : Int) . λ (One : Int) . One"

example4 :: IO ()
example4 =
  example
    "test/Morte/example4.l3"
    "π (a : *) . π (x : a) . π (y : a) . a"
    "λ (a : *) . λ (x : a) . λ (y : a) . y"

example5 :: IO ()
example5 =
  example
    "test/Morte/example5.l3"
    "π (r : *) . r . r . r"
    "λ (r : *) . λ (x : r) . λ (_ : r) . x"

example6 :: IO ()
example6 =
  example
    "test/Morte/example6.l3"
    "π (a : *) . (π (x : *) . (a . x . x) . x . x) . π (x : *) . (a . x . x) . x . x"
    "λ (a : *) . λ (l : π (x : *) . (a . x . x) . x . x) . l"

example7 :: IO ()
example7 =
  example
    "test/Morte/example7.l3"
    "π (a : *) . (π (x : *) . (a . x . x) . x . x) . π (x : *) . (a . x . x) . x . x"
    "λ (a : *) . λ (va : π (x : *) . (a . x . x) . x . x) . va"

example8 :: IO ()
example8 =
  example
    "test/Morte/example8.l3"
    "π (a : *) . π (b : *) . π (c : *) . π (f : b . c) . π (g : a . b) . (π (x : *) . (a . x . x) . x . x) . π (x : *) . (c . x . x) . x . x"
    "λ (a : *) . λ (b : *) . λ (c : *) . λ (f : b . c) . λ (g : a . b) . λ (l : π (x : *) . (a . x . x) . x . x) . λ (x : *) . λ (Cons : c . x . x) . l x (λ (va : a) . Cons (f (g va)))"

example9 :: IO ()
example9 =
  example
    "test/Morte/example9.l3"
    "π (a : *) . π (b : *) . π (c : *) . π (f : b . c) . π (g : a . b) . (π (x : *) . (a . x . x) . x . x) . π (x : *) . (c . x . x) . x . x"
    "λ (a : *) . λ (b : *) . λ (c : *) . λ (f : b . c) . λ (g : a . b) . λ (va : π (x : *) . (a . x . x) . x . x) . λ (x : *) . λ (Cons : c . x . x) . va x (λ (va : a) . Cons (f (g va)))"

example10 :: IO ()
example10 =
  example
    "test/Morte/example10.l3"
    "π (a : *) . (π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x"
    "λ (a : *) . λ (st : π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . st"

example11 :: IO ()
example11 =
  example
    "test/Morte/example11.l3"
    "π (a : *) . (π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x"
    "λ (a : *) . λ (va : π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . va"

example12 :: IO ()
example12 =
  example
    "test/Morte/example12.l3"
    "π (a : *) . π (b : *) . π (c : *) . (b . c) . (a . b) . (π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . π (x : *) . (π (s : *) . s . (s . π (x : *) . (c . s . x) . x) . x) . x"
    "λ (a : *) . λ (b : *) . λ (c : *) . λ (f : b . c) . λ (g : a . b) . λ (st : π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . λ (x : *) . λ (S : π (s : *) . s . (s . π (x : *) . (c . s . x) . x) . x) . st x (λ (s : *) . λ (seed : s) . λ (step : s . π (x : *) . (a . s . x) . x) . S s seed (λ (seed : s) . λ (x : *) . λ (Pair : c . s . x) . step seed x (λ (va : a) . Pair (f (g va)))))"

example13 :: IO ()
example13 =
  example
    "test/Morte/example13.l3"
    "π (a : *) . π (b : *) . π (c : *) . (b . c) . (a . b) . (π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . π (x : *) . (π (s : *) . s . (s . π (x : *) . (c . s . x) . x) . x) . x"
    "λ (a : *) . λ (b : *) . λ (c : *) . λ (f : b . c) . λ (g : a . b) . λ (va : π (x : *) . (π (s : *) . s . (s . π (x : *) . (a . s . x) . x) . x) . x) . λ (x : *) . λ (S : π (s : *) . s . (s . π (x : *) . (c . s . x) . x) . x) . va x (λ (s : *) . λ (seed : s) . λ (step : s . π (x : *) . (a . s . x) . x) . S s seed (λ (seed : s) . λ (x : *) . λ (Pair : c . s . x) . step seed x (λ (va : a) . Pair (f (g va)))))"

example14 :: IO ()
example14 =
  example
    "test/Morte/example14.l3"
    "π (String : *) . π (U : *) . π (Unit : U) . π (x : *) . (String . x . x) . ((String . x) . x) . (U . x) . x"
    "λ (String : *) . λ (U : *) . λ (Unit : U) . λ (x : *) . λ (PutStrLn : String . x . x) . λ (GetLine : (String . x) . x) . λ (Return : U . x) . GetLine (λ (va : String) . PutStrLn va (GetLine (λ (va : String) . PutStrLn va (Return Unit))))"

example15 :: IO ()
example15 =
  example
    "test/Morte/example15.l3"
    "π (String : *) . π (r : *) . π (x : *) . (π (s : *) . s . (s . π (x : *) . (String . s . x) . ((String . s) . x) . (r . x) . x) . x) . x"
    "λ (String : *) . λ (r : *) . λ (x : *) . λ (k : π (s : *) . s . (s . π (x : *) . (String . s . x) . ((String . s) . x) . (r . x) . x) . x) . k (π (x : *) . (String . x) . x . x) (λ (x : *) . λ (Just : String . x) . λ (Nothing : x) . Nothing) (λ (m : π (x : *) . (String . x) . x . x) . m (π (x : *) . (String . (π (x : *) . (String . x) . x . x) . x) . ((String . π (x : *) . (String . x) . x . x) . x) . (r . x) . x) (λ (str : String) . λ (x : *) . λ (PutStrLn : String . (π (x : *) . (String . x) . x . x) . x) . λ (GetLine : (String . π (x : *) . (String . x) . x . x) . x) . λ (Return : r . x) . PutStrLn str (λ (x : *) . λ (Just : String . x) . λ (Nothing : x) . Nothing)) (λ (x : *) . λ (PutStrLn : String . (π (x : *) . (String . x) . x . x) . x) . λ (GetLine : (String . π (x : *) . (String . x) . x . x) . x) . λ (Return : r . x) . GetLine (λ (va : String) . λ (x : *) . λ (Just : String . x) . λ (Nothing : x) . Just va)))"
