{-# LANGUAGE DeriveTraversable #-} -- derive Traversable, Functor, Foldable
{-# LANGUAGE TupleSections #-}

-- | Type checking and type inference
module L3.Core (module L3.Core, module L3.Util) where
    import L3.Util

    import Data.List (intercalate)
    import Data.Char (isDigit)


    -- | An expression in the calculus of constructions.
    data Expr a = Star
                | Box
                | Var a
                | Lam a (Expr a) (Expr a)
                | Pi a (Expr a) (Expr a)
                | App (Expr a) (Expr a)
                deriving (Eq, Show, Traversable, Functor, Foldable)

    -- | A context is a stack, mapping names to bound values.
    type Context a = [(a, Expr a)]

    newtype Name = Name String deriving (Eq)
    instance Show Name where
        show (Name s) = s

    type ShowExpr = Expr Name
    -- | Show an expression
    showExpr :: ShowExpr -> String
    showExpr Star      = "*"
    showExpr Box       = "#"
    showExpr (Var (Name i)) = i
    showExpr (Lam (Name i) typ e) = "lambda (" ++ i ++ " : " ++ showExpr typ ++ ") -> " ++ showExpr e
    showExpr (Pi (Name i) typ e)  = "forall (" ++ i ++ " : " ++ showExpr typ ++ ") -> " ++ showExpr e
    showExpr (App e expr)         = "(" ++ showExpr e ++ ") (" ++ showExpr expr ++ ")"

    type ShowCtx = Context Name
    -- | Show a context
    prettyShowCtx :: ShowCtx -> String
    prettyShowCtx ctx = intercalate ", " (map (\(Name n, typ) -> n ++ " : " ++ showExpr typ) ctx)

    -- | Show for a context, printing each binding on a separate line.
    showCtx :: (Show a) => Context a -> String
    showCtx (c:cs) = "\n" ++ show c ++ showCtx cs
    showCtx [] = ""


    -- | Is a name 'free' in an expression
    free :: (Eq a) => a -> Expr a -> Bool
    free v (Var v')               = v == v'
    free v (Lam v' _ _) | v == v' = True
    free v (Lam _ ta b)           = free v ta && free v b
    free v (Pi v' _  _) | v == v' = True
    free v (Pi _ ta b )           = free v ta && free v b
    free v (App f a   )           = free v f && free v a
    free _ _                      = True

    -- | Substitute all occurrences of a variable v with an expression e.
    -- | substitute v e E  ~  E[v := e]
    substitute :: (Eq a) => a -> Expr a -> Expr a -> Expr a
    substitute v e (Var v')       | v == v' = e
    substitute v e (Lam v' ta b ) | v == v' = Lam v' (substitute v e ta)            b
    substitute v e (Lam v' ta b )           = Lam v' (substitute v e ta) (substitute v e b )
    substitute v e (Pi  v' ta tb) | v == v' = Pi  v' (substitute v e ta)            tb
    substitute v e (Pi  v' ta tb)           = Pi  v' (substitute v e ta) (substitute v e tb)
    substitute v e (App f a     )           = App    (substitute v e f ) (substitute v e a )
    substitute _ _ e'                       = e'

    -- | Given an expression, reduce it one step towards its normal form.
    normalize :: (Eq a) => Expr a -> Expr a
    normalize (Lam v ta b) = case normalize b of
        App vb (Var v') | v == v' && not (free v vb) -> vb
        b' -> Lam v (normalize ta) b'
    normalize (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
    normalize (App f a) = case normalize f of
        Lam v _ b -> substitute v (normalize a) b
        f' -> App f' (normalize a)
    normalize c = c

    -- | Given an expression, totally reduce it over all steps towards normal form,
    -- | returning this normal.
    normalize0 :: (Eq a) => Expr a -> Expr a
    normalize0 e = case normalize e of
      e' | e == e' -> e
      e'           -> normalize0 e'

    -- | Given an 'free' index, convert an expression in Right names into Left indexes.
    -- | This uses DeBruijn indicies.
    index :: Eq a => Int -> Expr (Either Int a) -> Expr (Either Int a)
    index _ (Var v     ) = Var v
    index i (Lam v ta b) = Lam (Left i) (index i ta) (index (i + 1) $ substitute v (Var $ Left i) b)
    index i (Pi v ta tb) = Pi  (Left i) (index i ta) (index (i + 1) $ substitute v (Var $ Left i) tb)
    index i (App f a   ) = App (index i f) (index i  a)
    index _ Star = Star
    index _ Box  = Box

    -- | Provide an initial 'free' index of 0 and index an expression.
    -- | This converts any expression to its DeBruijn indexed form, leaving global
    -- | names untouched.
    index0 :: Eq a => Expr a -> Expr (Either Int a)
    index0 e = index 0 (fmap Right e)

    -- | Deduce whether two expressions are equivalent by converting to indexed form
    -- | and checking for exact equality. This does not apply normalisation, so
    -- | represents only alpha-equivalence of expressions.
    alphaEq :: (Eq a) => Expr a -> Expr a -> Bool
    alphaEq e e' = index0 e == index0 e'

    -- | Deduce whether two expressions are equivalent by converting to indexed form
    -- | and checking for exact equality. This does apply normalisation, so represents
    -- | beta-equivalence (and implicitly alpha-equivalence) of expressions.
    betaEq :: (Eq a) => Expr a -> Expr a -> Bool
    betaEq e e' = normalize0 e `alphaEq` normalize0 e'

    -- | Evaluate an expression, returning its type and normalized form
    evalExpr1 :: (Eq a, Show a) => Context a -> Expr a -> Result (Expr a, Expr a)
    evalExpr1 tCtx e = mapR (, normalize e) (inferType tCtx e)

    -- | Type-check an expression and return the expression's type if type-checking
    -- | succeeds or an error message if type-checking fails
    -- | `inferType` does not necessarily normalize the type since full normalization
    -- | is not necessary for just type-checking.  If you actually care about the
    -- | returned type then you may want to `normalize` it afterwards.
    -- | Type inference is within a type context (list of global names and their types)
    inferType :: (Eq a, Show a) => Context a -> Expr a -> Result (Expr a)
    inferType _ Star       = return Box
    inferType tCtx Box     = throwError $ "in context: " ++ showCtx tCtx ++ "\n absurd box"
    inferType tCtx (Var v) = case lookup v tCtx of
        Nothing -> throwError $ "in context: " ++ showCtx tCtx ++ "\n unbound variable: " ++ show v
        Just e  -> Right e
    inferType tCtx (Lam v ta b) = do
        tb <- inferType ((v, ta):tCtx) b
        let tf = Pi v ta tb
        -- Types may themselves be well-typed, since they are expressions
        _ <- inferType tCtx tf
        return tf
    inferType tCtx (Pi v ta tb) = do
        tta <- inferType tCtx ta
        ttb <- inferType ((v, ta):tCtx) tb
        case (tta, ttb) of
            (Star, Star) -> return Star
            (Box , Star) -> return Star
            (Star, Box ) -> return Box
            (Box , Box ) -> return Box
            (l   , r   ) -> throwError $ "invalid type: " ++ show (Pi v ta tb) ++ "\n had left kind: " ++ show l ++ "\n and right kind: " ++ show r
    inferType tCtx (App f a) = do
        (v, ta, tb) <- case inferType tCtx f of
            Right (Pi v ta tb) -> return (v, ta, tb)
            Right expr         -> throwError $ "cannot apply to non-function: " ++ show f ++ "\n had type: " ++ show expr ++ "\n had application: " ++ show a
            Left  err          -> throwError err
        ta' <- inferType tCtx a
        if ta `betaEq` ta'
            then return $ substitute v a tb
            else throwError $ "type mismatch for function: " ++ show f ++ "\n given arg: " ++ show a ++ "\n expected type: " ++ show ta ++ "\n but was type: " ++ show ta'

    -- | `inferType0` is the same as `inferType` with an empty context, meaning that
    -- | the expression must be closed (i.e. no free variables), otherwise type-checking
    -- | will fail.
    inferType0 :: (Eq a, Show a) => Expr a -> Result (Expr a)
    inferType0 = inferType []

    -- | Deduce if an expression e is well-typed - i.e. its type can be inferred.
    wellTyped :: (Eq a, Show a) => Context a -> Expr a -> Bool
    wellTyped tCtx e = case inferType tCtx e of
        Left _ -> False
        Right _ -> True

    -- | Deduce if an expression is well-typed context-free - i.e. it is additionally
    -- | closed and therefore well-typed without additional context.
    wellTyped0 :: (Eq a, Show a) => Expr a -> Bool
    wellTyped0 = wellTyped []
