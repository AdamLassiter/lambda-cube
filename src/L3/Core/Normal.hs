-- | Type checking and type inference
module L3.Core.Normal (free, fresh, substitute, normalize, normalize0) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.Expr
import L3.Core.Show
import L3.Log
import L3.Util

trace = traceU "Core::Normal"

-- | Is a name 'free' in an expression
--  In this context, free v a & v /= v'  =>  substitute v v' a /= a
--  i.e. would a substitution be performed
free :: (Eq a, Show a) => a -> Expr a -> Bool
free v e = trace ("free " ++ show v ++ ", " ++ show e) (free' v e)

free' v (Var v') = v == v'
free' v (Lam v' ta _) | v == v' = free v ta
free' v (Lam _ ta b) = free v ta || free v b
free' v (Pi v' ta _) | v == v' = free v ta
free' v (Pi _ ta tb) = free v ta || free v tb
free' v (App f a) = free v f || free v a
free' _ _ = False

fresh :: (Eq a, Enum a, Show a) => a -> Expr a -> a
fresh v e = trace ("fresh " ++ show v ++ ", " ++ show e) (fresh' v e)

fresh' from expr = v
  where
    enums e = succ e : enums (succ e)
    nonfree = filter (not . (`free` expr)) (enums from)
    v = head nonfree

-- | Substitute all occurrences of a variable v with an expression e.
--  substitute v e E  ~  E[v := e]
substitute :: (Eq a, Enum a, Show a) => a -> Expr a -> Expr a -> Expr a
substitute v e e' = trace ("substitute " ++ show v ++ ", " ++ show e ++ ", " ++ show e') (substitute' v e e')

substitute' v e (Var v') | v == v' = e
substitute' v e (Lam v' ta b) | v == v' = Lam v' (substitute v e ta) b
substitute' v e (Lam v' ta b) | free v' e = substitute v e (Lam v'' ta (substitute v' (Var v'') b))
  where
    v'' = fresh v' b
substitute' v e (Lam v' ta b) = Lam v' (substitute v e ta) (substitute v e b)
substitute' v e (Pi v' ta tb) | v == v' = Pi v' (substitute v e ta) tb
substitute' v e (Pi v' ta tb) | free v' e = substitute v e (Pi v'' ta (substitute v' (Var v'') tb))
  where
    v'' = fresh v' tb
substitute' v e (Pi v' ta tb) = Pi v' (substitute v e ta) (substitute v e tb)
substitute' v e (App f a) = App (substitute v e f) (substitute v e a)
substitute' _ _ e' = e'

-- | Given an expression, reduce it one step towards its normal form.
normalize :: (Eq a, Enum a, Show a) => Expr a -> Expr a
normalize e = trace ("normalize " ++ show e) (normalize' e)

normalize' (Lam v ta b) = case normalize b of
  App vb (Var v') | v == v' && not (free v vb) -> vb -- ε-reduction
  b' -> Lam v (normalize ta) (normalize b')
normalize' (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
normalize' (App f a) = case normalize f of
  Lam v _ b -> substitute v (normalize a) b
  f' -> App f' (normalize a)
normalize' c = c

-- | Given an expression, totally reduce it over all steps towards normal form.
normalize0 :: (Eq a, Enum a, Show a) => Expr a -> Expr a
normalize0 e = trace ("normalize0 " ++ show e) (normalize0' e)

normalize0' e = case normalize e of
  e' | e == e' -> e
  e' -> normalize0 e'
