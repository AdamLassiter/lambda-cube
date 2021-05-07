{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}

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

    free :: (Eq a, Enum a) => a -> Expr a -> Bool
    free v e = e /= substitute v (Var $ pred v) e

    -- Substitute all occurrences of a variable v with an expression e
    -- substitute x n C B  ~  B[x@n := C]
    substitute :: (Eq a, Enum a) => a -> Expr a -> Expr a -> Expr a
    substitute v e (Var v')       | v == v' = e
    substitute v e (Lam v' ta b ) | v == v' = Lam v' (substitute v e ta)            b
    substitute v e (Lam v' ta b )           = Lam v' (substitute v e ta) (substitute v e b )
    substitute v e (Pi  v' ta tb) | v == v' = Pi  v' (substitute v e ta)            tb
    substitute v e (Pi  v' ta tb)           = Pi  v' (substitute v e ta) (substitute v e tb)
    substitute v e (App f a     )           = App    (substitute v e f ) (substitute v e a )
    substitute v e e'                       = e'

    normalize :: (Eq a, Enum a) => Expr a -> Expr a
    normalize (Lam v ta b) = case normalize b of
        App vb (Var v') | v == v' && not (free v vb) -> vb
        b' -> Lam v (normalize ta) b'
    normalize (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
    normalize (App f a) = case normalize f of
        Lam v _ b -> substitute v (normalize a) b
        f' -> App f' (normalize a)
    normalize c = c

    equivalent :: (Eq a, Enum a) => a -> Expr a -> Expr a -> Bool
    equivalent n e e' = igo (normalize e) (normalize e') n where
        igo (Lam v ta b) (Lam v' ta' b') n = igo ta ta' n && igo (substitute v (Var n)  b) (substitute v' (Var n)  b') (pred n)
        igo (Pi v ta tb) (Pi v' ta' tb') n = igo ta ta' n && igo (substitute v (Var n) tb) (substitute v' (Var n) tb') (pred n)
        igo (App f a) (App f' a') n = igo f f' n && igo a a' n
        igo c c' n = c == c'

    equivalent0 :: (Eq a, Enum a) => Expr a -> Expr a -> Bool
    equivalent0 = equivalent (toEnum 0)

    -- Type-check an expression and return the expression's type if type-checking
    -- succeeds or Nothing if type-checking fails
    -- `inferType` does not necessarily normalize the type since full normalization
    -- is not necessary for just type-checking.  If you actually care about the
    -- returned type then you may want to `normalize` it afterwards.
    inferType :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Result (Expr a)
    inferType _ Star = return Box
    inferType _ Box  = throwError "absurd box"
    inferType ctx (Var v) = case lookup v ctx of
        Nothing -> throwError $ "unbound variable: " ++ show v
        Just e  -> Right e
    inferType ctx (Lam v ta b) = do
        tb <- inferType ((v, ta):ctx) b
        let tf = Pi v ta tb
        ttf <- inferType ctx tf
        return tf
    inferType ctx (Pi v ta tb) = do
        tta <- inferType ctx ta
        ttb <- inferType ((v, ta):ctx) tb
        case (tta, ttb) of
            (Star, Star) -> return Star
            (Box , Star) -> return Star
            (Star, Box ) -> return Box
            (Box , Box ) -> return Box
            (l   , r   ) -> throwError $ "invalid type: " ++ show (Pi v ta tb) ++ " ~> " ++ show (l, r)
    inferType ctx (App f a) = do
        (v, ta, tb) <- case inferType ctx f of
            Right (Pi v ta tb) -> return (v, ta, tb)
            Right expr         -> throwError $ "not a function: " ++ show (App f a) ++ " ~> " ++ show expr
            Left  err          -> throwError err
        ta' <- inferType ctx a
        if ta `equivalent0` ta'
          then return $ substitute v a tb
          else throwError $ "type mismatch: " ++ show ta ++ " != " ++ show ta'

    -- `inferType0` is the same as `inferType` with an empty context, meaning that the
    -- expression must be closed (i.e. no free variables), otherwise type-checking
    -- will fail.
    inferType0 :: (Eq a, Enum a, Show a) => Expr a -> Result (Expr a)
    inferType0 = inferType []

    -- Deduce if an expression e is well-typed
    wellTyped :: (Eq a, Enum a, Show a) => Context a -> Expr a -> Bool
    wellTyped ctx e = case inferType ctx e of
        Left _ -> False
        Right _ -> True

    wellTyped0 :: (Eq a, Enum a, Show a) => Expr a -> Bool
    wellTyped0 = wellTyped []
