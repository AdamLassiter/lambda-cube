-- | Type checking and type inference
module L3.Core.Infer (weakInferType, inferType, inferType0, inferType1, wellTyped, wellTyped0) where

import Data.Char (isDigit)
import Data.List (intercalate)
import L3.Core.Eq
import L3.Core.Expr
import L3.Core.Normal
import L3.Log
import L3.Util

trace = traceU "Core::Infer"

-- | Type-check an expression and return the expression's type if type-checking
-- | succeeds or an error message if type-checking fails
-- | `inferType'` does not necessarily normalize the type since full normalization
-- | is not necessary for just type-checking.  If you actually care about the
-- | returned type then you may want to `normalize` it afterwards.
-- | Type inference is within a type context (list of global names and their types)
-- |
-- | 'Weak' type infernce here refers to the lack of partial evaluation for contextual
-- | beta-equivalence. For some X, a by-value and by-reference of X should be legal:
-- |   (λ (T : *) . λ (f : π (X : *) . X) . λ (x : T) . f x) X
-- | In fact, resolving T := X as beta-equivalent to X will fail for weakInferType.
weakInferType :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a)
weakInferType (Ctx τ) e = trace ("weakInferType " ++ show τ ++ ", " ++ show e) (weakInferType' (Ctx τ) e)

weakInferType' _ Star = return Box
weakInferType' (Ctx τ) Box =
  Left $
    rethrowError
      ("in context:" : map showIndent τ)
      ( throwError
          [ "absurd box"
          ]
      )
weakInferType' (Ctx τ) (Var v) = case lookup v τ of
  Nothing ->
    Left $
      rethrowError
        ("in context:" : map showIndent τ)
        ( throwError
            [ "unbound variable:",
              showIndent v
            ]
        )
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
    (l, r) ->
      Left $
        rethrowError
          ("in context:" : map showIndent τ)
          ( throwError
              [ "invalid type:",
                showIndent (Pi v ta tb),
                "had left kind:",
                showIndent l,
                "had right kind:",
                showIndent r
              ]
          )
weakInferType' (Ctx τ) (App f a) = do
  (v, ta, tb) <- case weakInferType (Ctx τ) f of
    Right (Pi v ta tb) -> return (v, ta, tb)
    Right expr ->
      Left $
        rethrowError
          ("in context:" : map showIndent τ)
          ( throwError
              [ "cannot apply to non-function:",
                showIndent f,
                "had type: ",
                showIndent expr,
                "had application:",
                showIndent a
              ]
          )
    Left err -> Left $ matchBind f
      where
        matchBind (Lam v ta b) = rethrowError ["with binding:", showIndent (v, a)] err
        matchBind f = rethrowError ["in expression:", showIndent $ App f a] err
  ta' <- weakInferType (Ctx τ) a
  if ta `betaEq` ta'
    then return $ substitute v a tb
    else
      Left $
        rethrowError
          ("in context:" : map showIndent τ)
          ( throwError
              [ "type mismatch for function:",
                showIndent f,
                "expected type:",
                showIndent ta,
                "but given arg:",
                showIndent a,
                "and given type:",
                showIndent ta'
              ]
          )

-- | Type-check an expression and return the expression's normalized type if
-- | type-checking succeeds or an error message if type-checking fails
-- | Perform partial evaluation by substitution of lambda-applications to types
-- | to ensure the problem-case for `weakInferType` does not fail here.
inferType1 :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a)
inferType1 (Ctx τ) e = trace ("inferType1 " ++ show τ ++ ", " ++ show e) (inferType1' (Ctx τ) e)

inferType1' τ e = weakInferType τ e

-- | Type-check an expression and return the expression's normalized type if
-- | type-checking succeeds or an error message if type-checking fails
inferType :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a)
inferType (Ctx τ) e = trace ("inferType " ++ show τ ++ ", " ++ show e) (inferType' (Ctx τ) e)

inferType' τ e = mapR normalize $ inferType1 τ e

-- | `inferType0` is the same as `inferType` with an empty context, meaning that
-- | the expression must be closed (i.e. no free variables), otherwise type-checking
-- | will fail.
inferType0 :: (Eq a, Enum a, Show a) => Expr a -> Result (Expr a)
inferType0 e = trace ("inferType0 " ++ show e) (inferType0' e)

inferType0' :: (Eq a, Enum a, Show a) => Expr a -> Result (Expr a)
inferType0' = inferType (Ctx [])

-- | Deduce if an expression e is well-typed - i.e. its type can be inferred.
wellTyped :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Bool
wellTyped (Ctx τ) e = trace ("wellTyped " ++ show τ ++ ", " ++ show e) (wellTyped' (Ctx τ) e)

wellTyped' τ e = case inferType τ e of
  Left _ -> False
  Right _ -> True

-- | Deduce if an expression is well-typed context-free - i.e. it is additionally
-- | closed and therefore well-typed without additional context.
wellTyped0 :: (Eq a, Enum a, Show a) => Expr a -> Bool
wellTyped0 e = trace ("wellTyped0 " ++ show e) (wellTyped0' e)

wellTyped0' :: (Eq a, Enum a, Show a) => Expr a -> Bool
wellTyped0' = wellTyped (Ctx [])
