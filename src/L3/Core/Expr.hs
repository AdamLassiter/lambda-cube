-- derive Traversable, Functor, Foldable
{-# LANGUAGE DeriveTraversable #-}

module L3.Core.Expr where

import Data.Char (isDigit)
import Data.List (intercalate)

-- | An expression in the calculus of constructions.
data Expr a
  = Star
  | Box
  | Var a
  | Lam a (Expr a) (Expr a)
  | Pi a (Expr a) (Expr a)
  | App (Expr a) (Expr a)
  deriving (Eq, Traversable, Functor, Foldable)

-- | A context is a stack, mapping names to bound values.
data Context a = Ctx [(a, Expr a)]
