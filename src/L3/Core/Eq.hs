-- | Type checking and type inference
module L3.Core.Eq (alphaEq, betaEq) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.DeBruijn
import L3.Core.Expr
import L3.Core.Normal
import L3.Log.Logging
import L3.Util

trace = traceU "Core::Eq"

-- | Deduce whether two expressions are equivalent by converting to indexed form
--  and checking for exact equality. This does not apply normalisation, so
--  represents only alpha-equivalence of expressions.
alphaEq :: (Eq a, Enum a, Show a) => Expr a -> Expr a -> Bool
alphaEq e e' = trace ("alphaEq " ++ show e ++ ", " ++ show e') (alphaEq' e e')

alphaEq' e e' = index0 e == index0 e'

-- | Deduce whether two expressions are equivalent by converting to indexed form
--  and checking for exact equality. This does apply normalisation, so represents
--  beta-equivalence (and implicitly alpha-equivalence) of expressions.
betaEq :: (Eq a, Enum a, Show a) => Expr a -> Expr a -> Bool
betaEq e e' = trace ("betaEq " ++ show e ++ ", " ++ show e') (betaEq' e e')

betaEq' e e' = normalize0 e `alphaEq` normalize0 e'