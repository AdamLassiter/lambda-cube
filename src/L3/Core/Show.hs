{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Type checking and type inference
module L3.Core.Show where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.Expr
import L3.Log
import L3.Util

newtype Name = Name String deriving (Eq)

instance Enum Name where
  fromEnum (Name ('ζ' : digits)) = read digits
  fromEnum (Name x) = 0
  toEnum int = Name $ "ζ" ++ show int

instance Show Name where
  show (Name s) = s

instance (Enum a, Enum b) => Enum (Either a b) where
  fromEnum (Left l) = 2 * fromEnum l
  fromEnum (Right r) = 2 * fromEnum r + 1
  toEnum i | even i = Left $ toEnum $ i `div` 2
  toEnum i = Right $ toEnum $ (i - 1) `div` 2

type ShowExpr = Expr Name

-- | Show an expression
showExpr :: (Show a) => Expr a -> String
showExpr Star = "*"
showExpr Box = "#"
showExpr (Var i) = show i
showExpr (Lam i typ e) = "λ [" ++ show i ++ " : " ++ showExpr typ ++ "] -> " ++ showExpr e
showExpr (Pi i typ e) = "π [" ++ show i ++ " : " ++ showExpr typ ++ "] -> " ++ showExpr e
showExpr (App e expr) = "(" ++ showExpr e ++ ") (" ++ showExpr expr ++ ")"

instance Show a => Show (Expr a) where
  show = showExpr

type ShowCtx = Context Name

-- | Show for a context, printing each binding on a separate line.
showCtx :: (Show a) => Context a -> String
showCtx (Ctx (c : cs)) = "\n" ++ show c ++ showCtx (Ctx cs)
showCtx (Ctx []) = ""

instance (Show a) => Show (Context a) where
  show = showCtx
