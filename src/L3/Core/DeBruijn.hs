-- | Type checking and type inference
module L3.Core.DeBruijn (index, index0) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.Expr
import L3.Core.Show
import L3.Log
import L3.Util

trace = traceU "Core::DeBruijn"

-- | Given an 'free' index, convert an expression in Right names into Left indexes.
--  This uses DeBruijn indicies.
index :: (Eq a, Enum a, Show a) => Int -> Expr (Either Int a) -> Expr (Either Int a)
index i e = trace ("index " ++ show i ++ ", " ++ show e) (index' i e)

index' i = indexWith i []

indexWith :: (Eq a, Enum a, Show a) => Int -> [(Either Int a, Int)] -> Expr (Either Int a) -> Expr (Either Int a)
indexWith _ env (Var v) = case lookup v env of
  Nothing -> Var v
  Just i -> Var $ Left i
indexWith i env (Lam v ta b) = Lam (Left i) (indexWith i env ta) (indexWith (i + 1) ((v, i) : env) b)
indexWith i env (Pi v ta tb) = Pi (Left i) (indexWith i env ta) (indexWith (i + 1) ((v, i) : env) tb)
indexWith i env (App f a) = App (indexWith i env f) (indexWith i env a)
indexWith _ _ Star = Star
indexWith _ _ Box = Box

-- | Provide an initial 'free' index of 0 and index an expression.
--  This converts any expression to its DeBruijn indexed form, leaving global
--  names untouched.
index0 :: (Eq a, Enum a, Show a) => Expr a -> Expr (Either Int a)
index0 e = trace ("index0 " ++ show e) (index0' e)

index0' e = index 0 (fmap Right e)
