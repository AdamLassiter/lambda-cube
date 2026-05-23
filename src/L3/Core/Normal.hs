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

free' :: Eq a => a -> Expr a -> Bool
free' v (Var v') = v == v'
free' v (Lam v' ta _) | v == v' = free' v ta
free' v (Lam _ ta b) = free' v ta || free' v b
free' v (Pi v' ta _) | v == v' = free' v ta
free' v (Pi _ ta tb) = free' v ta || free' v tb
free' v (App f a) = free' v f || free' v a
free' _ _ = False

fresh :: (Eq a, Enum a, Show a) => a -> Expr a -> a
fresh v e = trace ("fresh " ++ show v ++ ", " ++ show e) (fresh' v e)

fresh' :: (Eq a, Enum a) => a -> Expr a -> a
fresh' from expr = v
  where
    enums e = succ e : enums (succ e)
    nonfree = filter (\candidate -> not $ free' candidate expr) (enums from)
    v = head nonfree

-- | Substitute all occurrences of a variable v with an expression e.
--  substitute v e E  ~  E[v := e]
substitute :: (Eq a, Enum a, Show a) => a -> Expr a -> Expr a -> Expr a
substitute v e e' = trace ("substitute " ++ show v ++ ", " ++ show e ++ ", " ++ show e') (substitute' v e e')

substitute' :: (Eq a, Enum a) => a -> Expr a -> Expr a -> Expr a
substitute' v e e' = maybe e' id (substituteMaybe v e e')

substituteMaybe :: (Eq a, Enum a) => a -> Expr a -> Expr a -> Maybe (Expr a)
substituteMaybe v e (Var v')
  | v == v' = Just e
  | otherwise = Nothing
substituteMaybe v e (Lam v' ta b)
  | v == v' = case substituteMaybe v e ta of
      Nothing -> Nothing
      Just ta' -> Just $ Lam v' ta' b
  | free' v b && free' v' e = Just $ Lam v'' (substitute' v e ta) (substitute' v e (substitute' v' (Var v'') b))
  where
    v'' = fresh' v' (App b e)
substituteMaybe v e (Lam v' ta b) = case (substituteMaybe v e ta, substituteMaybe v e b) of
  (Nothing, Nothing) -> Nothing
  (ta', b') -> Just $ Lam v' (maybe ta id ta') (maybe b id b')
substituteMaybe v e (Pi v' ta tb)
  | v == v' = case substituteMaybe v e ta of
      Nothing -> Nothing
      Just ta' -> Just $ Pi v' ta' tb
  | free' v tb && free' v' e = Just $ Pi v'' (substitute' v e ta) (substitute' v e (substitute' v' (Var v'') tb))
  where
    v'' = fresh' v' (App tb e)
substituteMaybe v e (Pi v' ta tb) = case (substituteMaybe v e ta, substituteMaybe v e tb) of
  (Nothing, Nothing) -> Nothing
  (ta', tb') -> Just $ Pi v' (maybe ta id ta') (maybe tb id tb')
substituteMaybe v e (App f a) = case (substituteMaybe v e f, substituteMaybe v e a) of
  (Nothing, Nothing) -> Nothing
  (f', a') -> Just $ App (maybe f id f') (maybe a id a')
substituteMaybe _ _ _ = Nothing

-- | Given an expression, reduce it one step towards its normal form.
normalize :: (Eq a, Enum a, Show a) => Expr a -> Expr a
normalize e = trace ("normalize " ++ show e) (normalize' e)

normalize' :: (Eq a, Enum a) => Expr a -> Expr a
normalize' = fst . normalizeStep

normalizeStep :: (Eq a, Enum a) => Expr a -> (Expr a, Bool)
normalizeStep (Lam v ta b) = case b' of
  App vb (Var v') | v == v' && not (free' v vb) -> (vb, True) -- ε-reduction
  _ -> (Lam v ta' b'', taChanged || bChanged || bAgainChanged)
  where
    (ta', taChanged) = normalizeStep ta
    (b', bChanged) = normalizeStep b
    (b'', bAgainChanged) =
      if bChanged
        then normalizeStep b'
        else (b', False)
normalizeStep (Pi v ta tb) = (Pi v ta' tb', taChanged || tbChanged)
  where
    (ta', taChanged) = normalizeStep ta
    (tb', tbChanged) = normalizeStep tb
normalizeStep (App f a) = case f' of
  Lam v _ b -> (substitute' v a' b, True)
  _ -> (App f' a', fChanged || aChanged)
  where
    (f', fChanged) = normalizeStep f
    (a', aChanged) = normalizeStep a
normalizeStep c = (c, False)

-- | Given an expression, totally reduce it over all steps towards normal form.
normalize0 :: (Eq a, Enum a, Show a) => Expr a -> Expr a
normalize0 e = trace ("normalize0 " ++ show e) (normalize0' e)

normalize0' :: (Eq a, Enum a) => Expr a -> Expr a
normalize0' = normalizeFull

normalizeFull :: (Eq a, Enum a) => Expr a -> Expr a
normalizeFull (Lam v ta b) = case normalizeFull b of
  App vb (Var v') | v == v' && not (free' v vb) -> vb -- ε-reduction
  b' -> Lam v (normalizeFull ta) b'
normalizeFull (Pi v ta tb) = Pi v (normalizeFull ta) (normalizeFull tb)
normalizeFull (App f a) = case normalizeFull f of
  Lam v _ b -> case substituteMaybe v (normalizeFull a) b of
    Nothing -> b
    Just b' -> normalizeFull b'
  f' -> App f' (normalizeFull a)
normalizeFull c = c
