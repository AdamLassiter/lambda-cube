{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

-- Type checking and type inference
module L3.Core where
    import L3.Util

    data Expr a = Star
                | Box
                | Var a
                | Lam a (Expr a) (Expr a)
                | Pi a (Expr a) (Expr a)
                | App (Expr a) (Expr a)
                deriving (Eq, Show, Traversable, Functor, Foldable)

    type Context a = [(a, Expr a)]

    free :: (Eq a) => a -> Expr a -> Bool
    free v (Var v')                = v == v'
    free v (Lam v' _ _ ) | v == v' = True
    free v (Lam v' ta b)           = free v ta && free v b
    free v (Pi v' _   _) | v == v' = True
    free v (Pi v' ta b )           = free v ta && free v b
    free v (App f a    )           = free v f && free v a

    -- Substitute all occurrences of a variable v with an expression e
    -- substitute x n C B  ~  B[x@n := C]
    substitute :: (Eq a) => a -> Expr a -> Expr a -> Expr a
    substitute v e (Var v')       | v == v' = e
    substitute v e (Lam v' ta b ) | v == v' = Lam v' (substitute v e ta)            b
    substitute v e (Lam v' ta b )           = Lam v' (substitute v e ta) (substitute v e b )
    substitute v e (Pi  v' ta tb) | v == v' = Pi  v' (substitute v e ta)            tb
    substitute v e (Pi  v' ta tb)           = Pi  v' (substitute v e ta) (substitute v e tb)
    substitute v e (App f a     )           = App    (substitute v e f ) (substitute v e a )
    substitute v e e'                       = e'

    normalize :: (Eq a) => Expr a -> Expr a
    normalize (Lam v ta b) = case normalize b of
        App vb (Var v') | v == v' && not (free v vb) -> vb
        b' -> Lam v (normalize ta) b'
    normalize (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
    normalize (App f a) = case normalize f of
        Lam v _ b -> substitute v (normalize a) b
        f' -> App f' (normalize a)
    normalize c = c

    normalize0 :: (Eq a, Show a) => Expr a -> Expr a
    normalize0 e = case normalize e of
      e' | e `equivalent0` e' -> e
      e'                      -> normalize0 e'

    index :: Eq a => Int -> Expr (Either Int a) -> Expr (Either Int a)
    index i (Var v)       = Var v
    index i (Lam v ta b ) = Lam (Left i) (index (i + 1) ta) (substitute v (Var $ Left i) ta)
    index i (Pi v ta b )  = Pi (Left i) (index (i + 1) ta) (substitute v (Var $ Left i) ta)
    index i (App f a    ) = App (index i f) (index i  a)
    index i Star = Star
    index i Box  = Box

    index0 :: Eq a => Expr a -> Expr (Either Int a)
    index0 e = index 0 (fmap Right e)

    equivalent0 :: (Eq a) => Expr a -> Expr a -> Bool
    equivalent0 e e' = index0 e == index0 e'

    -- evaluate the type and normalized form of an expression
    evalExpr1 :: (Eq a, Show a) => Context a -> Expr a -> Result (Expr a, Expr a)
    evalExpr1 tCtx e = mapR (, normExpr) typ
        where normExpr = normalize e
              typ = inferType tCtx e

    -- Type-check an expression and return the expression's type if type-checking
    -- succeeds or an error message if type-checking fails
    -- `inferType` does not necessarily normalize the type since full normalization
    -- is not necessary for just type-checking.  If you actually care about the
    -- returned type then you may want to `normalize` it afterwards.
    -- Type inference is within a type context (list of global names and their types)
    inferType :: (Eq a, Show a) => Context a -> Expr a -> Result (Expr a)
    inferType _ Star = return Box
    inferType _ Box  = throwError "absurd box"
    inferType tCtx (Var v) = case lookup v tCtx of
        Nothing -> throwError $ "unbound variable: " ++ show v ++ "\n in context: " ++ show tCtx
        Just e  -> Right e
    inferType tCtx (Lam v ta b) = do
        tb <- inferType ((v, ta):tCtx) b
        let tf = Pi v ta tb
        ttf <- inferType tCtx tf
        return tf
    inferType tCtx (Pi v ta tb) = do
        tta <- inferType tCtx ta
        ttb <- inferType ((v, ta):tCtx) tb
        case (tta, ttb) of
            (Star, Star) -> return Star
            (Box , Star) -> return Star
            (Star, Box ) -> return Box
            (Box , Box ) -> return Box
            (l   , r   ) -> throwError $ "invalid type: " ++ show (Pi v ta tb) ++ "\n had kind: " ++ show (l, r)
    inferType tCtx (App f a) = do
        (v, ta, tb) <- case inferType tCtx f of
            Right (Pi v ta tb) -> return (v, ta, tb)
            Right expr         -> throwError $ "cannot apply to non-function: " ++ show (App f a) ++ "\n had application: " ++ show expr
            Left  err          -> throwError err
        ta' <- inferType tCtx a
        if ta `equivalent0` ta'
            then return $ substitute v a tb
            else throwError $ "type mismatch for function and arg: " ++ show (App f a) ++ "\n expected: " ++ show ta ++ "\n but got: " ++ show ta'

    -- `inferType0` is the same as `inferType` with an empty context, meaning that the
    -- expression must be closed (i.e. no free variables), otherwise type-checking
    -- will fail.
    inferType0 :: (Eq a, Show a) => Expr a -> Result (Expr a)
    inferType0 = inferType []

    -- Deduce if an expression e is well-typed
    wellTyped :: (Eq a, Show a) => Context a -> Expr a -> Bool
    wellTyped tCtx e = case inferType tCtx e of
        Left _ -> False
        Right _ -> True

    wellTyped0 :: (Eq a, Show a) => Expr a -> Bool
    wellTyped0 = wellTyped []
