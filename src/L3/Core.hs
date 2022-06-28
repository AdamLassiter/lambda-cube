-- derive Traversable, Functor, Foldable
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Type checking and type inference
module L3.Core (module L3.Core, module L3.Util) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Logging
import L3.Util

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

debugCore = debugU "Core"

-- | Is a name 'free' in an expression
--  In this context, free v a & v /= v'  =>  substitute v v' a /= a
--  i.e. would a substitution be performed
free :: (Eq a, Show a) => a -> Expr a -> Bool
free v e = debugCore ("free " ++ show v ++ ", " ++ show e) (free' v e)

free' v (Var v') = v == v'
free' v (Lam v' ta _) | v == v' = free v ta
free' v (Lam _ ta b) = free v ta || free v b
free' v (Pi v' ta _) | v == v' = free v ta
free' v (Pi _ ta tb) = free v ta || free v tb
free' v (App f a) = free v f || free v a
free' _ _ = False

fresh :: (Eq a, Enum a, Show a) => a -> Expr a -> a
fresh v e = debugCore ("fresh " ++ show v ++ ", " ++ show e) (fresh' v e)

fresh' from expr = v
  where
    enums e = succ e : enums (succ e)
    nonfree = filter (not . (`free` expr)) (enums from)
    v = head nonfree

-- | Substitute all occurrences of a variable v with an expression e.
--  substitute v e E  ~  E[v := e]
substitute :: (Eq a, Enum a, Show a) => a -> Expr a -> Expr a -> Expr a
substitute v e e' = debugCore ("substitute " ++ show v ++ ", " ++ show e ++ ", " ++ show e') (substitute' v e e')

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
normalize e = debugCore ("normalize " ++ show e) (normalize' e)

normalize' (Lam v ta b) = case normalize b of
  App vb (Var v') | v == v' && not (free v vb) -> vb -- ε-reduction
  b' -> Lam v (normalize ta) (normalize b')
normalize' (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
normalize' (App f a) = case normalize f of
  Lam v _ b -> substitute v (normalize a) b
  f' -> App f' (normalize a)
normalize' c = c

-- | Given an expression, totally reduce it over all steps towards normal form,
--  returning this normal.
normalize0 :: (Eq a, Enum a, Show a) => Expr a -> Expr a
normalize0 e = debugCore ("normalize0 " ++ show e) (normalize0' e)

normalize0' e = case normalize e of
  e' | e == e' -> e
  e' -> normalize0 e'

-- | Given an 'free' index, convert an expression in Right names into Left indexes.
--  This uses DeBruijn indicies.
index :: (Eq a, Enum a, Show a) => Int -> Expr (Either Int a) -> Expr (Either Int a)
index i e = debugCore ("index " ++ show i ++ ", " ++ show e) (index' i e)

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
index0 e = debugCore ("index0 " ++ show e) (index0' e)

index0' e = index 0 (fmap Right e)

-- | Deduce whether two expressions are equivalent by converting to indexed form
--  and checking for exact equality. This does not apply normalisation, so
--  represents only alpha-equivalence of expressions.
alphaEq :: (Eq a, Enum a, Show a) => Expr a -> Expr a -> Bool
alphaEq e e' = debugCore ("alphaEq " ++ show e ++ ", " ++ show e') (alphaEq' e e')

alphaEq' e e' = index0 e == index0 e'

-- | Deduce whether two expressions are equivalent by converting to indexed form
--  and checking for exact equality. This does apply normalisation, so represents
--  beta-equivalence (and implicitly alpha-equivalence) of expressions.
betaEq :: (Eq a, Enum a, Show a) => Expr a -> Expr a -> Bool
betaEq e e' = debugCore ("betaEq " ++ show e ++ ", " ++ show e') (betaEq' e e')

betaEq' e e' = normalize0 e `alphaEq` normalize0 e'

-- | Evaluate an expression, returning its type and normalized form
evalExpr :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a, Expr a)
evalExpr (Ctx τ) e = debugCore ("evalExpr " ++ show τ ++ ", " ++ show e) (evalExpr' (Ctx τ) e)

evalExpr' τ e = mapR (,normalize e) (inferType τ e)

-- | Type-check an expression and return the expression's type if type-checking
--  succeeds or an error message if type-checking fails
--  `inferType'` does not necessarily normalize the type since full normalization
--  is not necessary for just type-checking.  If you actually care about the
--  returned type then you may want to `normalize` it afterwards.
--  Type inference is within a type context (list of global names and their types)
--
-- | 'Weak' type infernce here refers to the lack of partial evaluation for contextual
-- beta-equivalence. For some X, a by-value and by-reference of X should be legal:
--   (λ (T : *) . λ (f : π (X : *) . X) . λ (x : T) . f x) X
-- In fact, resolving T := X as beta-equivalent to X will fail for weakInferType.
weakInferType :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a)
weakInferType (Ctx τ) e = debugCore ("weakInferType " ++ show τ ++ ", " ++ show e) (weakInferType' (Ctx τ) e)

weakInferType' _ Star = return Box
weakInferType' (Ctx τ) Box = Left $ rethrowError ("in context:" : map showIdent τ) (throwError ["absurd box"])
weakInferType' (Ctx τ) (Var v) = case lookup v τ of
  Nothing -> Left $ rethrowError ("in context:" : map showIdent τ) (throwError ["unbound variable:", showIdent v])
  Just e -> Right e
weakInferType' (Ctx τ) (Lam v ta b) = do
  tb <- weakInferType (Ctx ((v, ta) : τ)) b
  let tf = Pi v ta tb
  -- Types may themselves be well-typed, since they are expressions
  _ <- weakInferType (Ctx τ) tf
  return tf
weakInferType' (Ctx τ) (Pi v ta tb) = do
  tta <- weakInferType (Ctx τ) ta
  ttb <- weakInferType (Ctx ((v, ta) : τ)) tb
  case (tta, ttb) of
    (Star, Star) -> return Star
    (Box, Star) -> return Star
    (Star, Box) -> return Box
    (Box, Box) -> return Box
    (l, r) -> Left $ rethrowError ("in context:" : map showIdent τ) (throwError ["invalid type:", showIdent (Pi v ta tb), "had left kind:", showIdent l, "had right kind:", showIdent r])
weakInferType' (Ctx τ) (App f a) = do
  (v, ta, tb) <- case weakInferType (Ctx τ) f of
    Right (Pi v ta tb) -> return (v, ta, tb)
    Right expr -> Left $ rethrowError ("in context:" : map showIdent τ) (throwError ["cannot apply to non-function:", showIdent f, "had type: ", showIdent expr, "had application:", showIdent a])
    Left err -> Left $ matchBind f
      where
        matchBind (Lam v ta b) = rethrowError ["with binding:", showIdent (v, a)] err
        matchBind f = rethrowError ["in expression:", showIdent $ App f a] err
  ta' <- weakInferType (Ctx τ) a
  if ta `betaEq` ta'
    then return $ substitute v a tb
    else Left $ rethrowError ("in context:" : map showIdent τ) (throwError ["type mismatch for function:", showIdent f, "expected type:", showIdent ta, "but given arg:", showIdent a, "and given type:", showIdent ta'])

-- | Type-check an expression and return the expression's normalized type if
--  type-checking succeeds or an error message if type-checking fails
-- Perform partial evaluation by substitution of lambda-applications to types
-- to ensure the problem-case for `weakInferType` does not fail here.
inferType1 :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a)
inferType1 (Ctx τ) e = debugCore ("inferType1 " ++ show τ ++ ", " ++ show e) (inferType1' (Ctx τ) e)

inferType1' τ e = weakInferType τ e

-- | Type-check an expression and return the expression's normalized type if
--  type-checking succeeds or an error message if type-checking fails
inferType :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a)
-- TODO : substitute for types, then infer types
inferType (Ctx τ) e = debugCore ("inferType " ++ show τ ++ ", " ++ show e) (inferType' (Ctx τ) e)

inferType' τ e = mapR normalize $ inferType1 τ e

-- | `inferType0` is the same as `inferType` with an empty context, meaning that
--  the expression must be closed (i.e. no free variables), otherwise type-checking
--  will fail.
inferType0 :: (Eq a, Enum a, Show a) => Expr a -> Result (Expr a)
inferType0 e = debugCore ("inferType0 " ++ show e) (inferType0' e)

inferType0' :: (Eq a, Enum a, Show a) => Expr a -> Result (Expr a)
inferType0' = inferType (Ctx [])

-- | Deduce if an expression e is well-typed - i.e. its type can be inferred.
wellTyped :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Bool
wellTyped (Ctx τ) e = debugCore ("wellTyped " ++ show τ ++ ", " ++ show e) (wellTyped' (Ctx τ) e)

wellTyped' τ e = case inferType τ e of
  Left _ -> False
  Right _ -> True

-- | Deduce if an expression is well-typed context-free - i.e. it is additionally
--  closed and therefore well-typed without additional context.
wellTyped0 :: (Eq a, Enum a, Show a) => Expr a -> Bool
wellTyped0 e = debugCore ("wellTyped0 " ++ show e) (wellTyped0' e)

wellTyped0' :: (Eq a, Enum a, Show a) => Expr a -> Bool
wellTyped0' = wellTyped (Ctx [])
