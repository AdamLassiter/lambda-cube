{-# LANGUAGE TupleSections #-}

-- | Type checking and type inference
module L3.Core.Eval (evalExpr, evalExpr0) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.Expr
import L3.Core.Infer
import L3.Core.Normal
import L3.Log
import L3.Util

debug = debugU "Core::Eval"

-- | Evaluate an expression, returning its type and normalized form
evalExpr :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a, Expr a)
evalExpr (Ctx τ) e = debug ("evalExpr " ++ show τ ++ ", " ++ show e) (evalExpr' (Ctx τ) e)

evalExpr' τ e = mapR (,normalize e) (inferType τ e)

-- | `evalExpr0` is the same as `evalExpr` with an empty context, meaning that
-- the expression must be closed (i.e. no free variables), otherwise evaluation
-- will fail.
evalExpr0 :: (Eq a, Enum a, Show a) => Expr a -> Result (Expr a, Expr a)
evalExpr0 e = debug ("evalExpr0 " ++ show e) (evalExpr0' e)

evalExpr0' :: (Eq a, Enum a, Show a) => Expr a -> Result (Expr a, Expr a)
evalExpr0' = evalExpr (Ctx [])
