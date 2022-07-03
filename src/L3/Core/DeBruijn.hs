-- | Type checking and type inference
module L3.Core.DeBruijn (index, index0) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.Expr
import L3.Core.Normal
import L3.Core.Show
import L3.Log
import L3.Util

trace = traceU "Core::DeBruijn"

-- | Given an 'free' index, convert an expression in Right names into Left indexes.
--  This uses DeBruijn indicies.
index :: (Eq a, Enum a, Show a) => Int -> Expr (Either Int a) -> Expr (Either Int a)
index i e = trace ("index " ++ show i ++ ", " ++ show e) (index' i e)

index' _ (Var v) = Var v
index' i (Lam v ta b) = Lam (Left i) (index i ta) (index (i + 1) $ substitute v (Var $ Left i) b)
index' i (Pi v ta tb) = Pi (Left i) (index i ta) (index (i + 1) $ substitute v (Var $ Left i) tb)
index' i (App f a) = App (index i f) (index i a)
index' _ Star = Star
index' _ Box = Box

-- | Provide an initial 'free' index of 0 and index an expression.
--  This converts any expression to its DeBruijn indexed form, leaving global
--  names untouched.
index0 :: (Eq a, Enum a, Show a) => Expr a -> Expr (Either Int a)
index0 e = trace ("index0 " ++ show e) (index0' e)

index0' e = index 0 (fmap Right e)
