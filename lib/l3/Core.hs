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

    type PartialContext a b = [(a, Expr b)]
    type Context a = PartialContext a a

    type DeBruijnExpr = Expr Int
    type DeBruijnCtx = Context Int


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

    -- Deduce if a variable v is free in an expression e
    free :: Int -> DeBruijnExpr -> Bool
    free v e = e /= substitute v (Var $ v + 1) e

    -- Reduce an expression to its normal form, performing both beta reduction and
    -- eta reduction
    -- `normalize` does not type-check the expression.  You may want to type-check
    -- expressions before normalizing them since normalization can convert an
    -- ill-typed expression into a well-typed expression.
    normalize :: DeBruijnExpr -> DeBruijnExpr
    normalize = converge normalize0

    normalize0 :: DeBruijnExpr -> DeBruijnExpr
    normalize0 (Lam v ta b) = case normalize b of
        App vb (Var v') | v == v' && not (free v vb) -> vb -- Eta reduce
        b'                                           -> Lam v (normalize ta) b'
    normalize0 (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
    normalize0 (App f a) = case normalize f of
        Lam v _ b -> substitute v (normalize a) b -- Beta reduce
        f'        -> App f' (normalize a)
    normalize0 c = c

    -- Deduce if e is an equivalentalent expression to e'
    equivalent :: DeBruijnExpr -> DeBruijnExpr -> Bool
    e `equivalent` e' = equivalent0 (normalize e) (normalize e') (-1)

    equivalent0 :: DeBruijnExpr -> DeBruijnExpr -> Int -> Bool
    equivalent0 (Lam v ta b) (Lam v' ta' b') n = equivalent0 ta ta' n && equivalent0 (substitute v (Var n)  b) (substitute v' (Var n)  b') (pred n)
    equivalent0 (Pi v ta tb) (Pi v' ta' tb') n = equivalent0 ta ta' n && equivalent0 (substitute v (Var n) tb) (substitute v' (Var n) tb') (pred n)
    equivalent0 (App f a)    (App f' a')     n = equivalent0 f f' n && equivalent0 a a' n
    equivalent0 c            c'              n = c == c'

    -- Type-check an expression and return the expression's type if type-checking
    -- suceeds or Nothing if type-checking fails
    -- `inferType` does not necessarily normalize the type since full normalization
    -- is not necessary for just type-checking.  If you actually care about the
    -- returned type then you may want to `normalize` it afterwards.
    inferType :: DeBruijnCtx -> DeBruijnExpr -> Result DeBruijnExpr
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
        if ta `equivalent` ta'
          then return $ substitute v a tb
          else throwError $ "type mismatch: " ++ show ta ++ " != " ++ show ta'

    -- `typeOf` is the same as `inferType` with an empty context, meaning that the
    -- expression must be closed (i.e. no free variables), otherwise type-checking
    -- will fail.
    typeOf :: DeBruijnExpr -> Result DeBruijnExpr
    typeOf = inferType []

    -- Deduce if an expression e is well-typed
    wellTyped :: DeBruijnExpr -> Bool
    wellTyped e = case typeOf e of
        Right _ -> True
        Left _  -> False
