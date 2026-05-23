-- | Type checking and type inference
module L3.Core.Eq (alphaEq, betaEq) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.Expr
import L3.Core.Normal
import L3.Log.Logging
import L3.Util

trace = traceU "Core::Eq"

-- | Deduce whether two expressions are alpha-equivalent without normalisation.
alphaEq :: (Eq a, Enum a, Show a) => Expr a -> Expr a -> Bool
alphaEq e e' = trace ("alphaEq " ++ show e ++ ", " ++ show e') (e == e' || alphaEq' e e')

alphaEq' :: Eq a => Expr a -> Expr a -> Bool
alphaEq' = alphaEqWith 0 [] []

alphaEqWith :: Eq a => Int -> [(a, Int)] -> [(a, Int)] -> Expr a -> Expr a -> Bool
alphaEqWith _ _ _ Star Star = True
alphaEqWith _ _ _ Box Box = True
alphaEqWith _ lEnv rEnv (Var v) (Var v') = case (lookup v lEnv, lookup v' rEnv) of
  (Just l, Just r) -> l == r
  (Nothing, Nothing) -> v == v'
  _ -> False
alphaEqWith i lEnv rEnv (Lam v ta b) (Lam v' ta' b') =
  alphaEqWith i lEnv rEnv ta ta'
    && alphaEqWith (i + 1) ((v, i) : lEnv) ((v', i) : rEnv) b b'
alphaEqWith i lEnv rEnv (Pi v ta tb) (Pi v' ta' tb') =
  alphaEqWith i lEnv rEnv ta ta'
    && alphaEqWith (i + 1) ((v, i) : lEnv) ((v', i) : rEnv) tb tb'
alphaEqWith i lEnv rEnv (App f a) (App f' a') =
  alphaEqWith i lEnv rEnv f f'
    && alphaEqWith i lEnv rEnv a a'
alphaEqWith _ _ _ _ _ = False

-- | Deduce whether two expressions are beta-equivalent by checking cheap
--  syntactic/alpha-equivalence shortcuts before normalising both expressions.
betaEq :: (Eq a, Enum a, Show a) => Expr a -> Expr a -> Bool
betaEq e e' = trace ("betaEq " ++ show e ++ ", " ++ show e') (betaEq' e e')

betaEq' e e' = e == e' || alphaEq' e e' || ne == ne' || alphaEq' ne ne'
  where
    ne = normalize0 e
    ne' = normalize0 e'
