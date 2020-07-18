module Constructions where

    import Util

    data Expr = Star
              | Box
              | Var Int
              | Lam Int Expr Expr
              | Pi Int Expr Expr
              | App Expr Expr deriving (Show, Eq)


    -- pretty-printing for expressions

    showExpr :: Expr -> String
    showExpr (Star)       = "*"
    showExpr (Box)        = "□"
    showExpr (Var i)      = show i
    showExpr (Lam i e e') = "λ " ++ show i ++ " : " ++ show e ++ " . " ++ show e'
    showExpr (Pi i e e')  = "Π " ++ show i ++ " : " ++ show e ++ " . " ++ show e'
    showExpr (App e ρ)    = "( " ++ show e ++ " ) ( " ++ show ρ ++ " )"


    -- type checking and type inference

    -- Substitute all occurrences of a variable v with an expression e
    -- subst x n C B  ~  B[x@n := C]
    subst :: Int -> Expr -> Expr -> Expr
    subst v e (Var v')       | v == v' = e
    subst v e (Lam v' ta b ) | v == v' = Lam v' (subst v e ta)            b
    subst v e (Lam v' ta b )           = Lam v' (subst v e ta) (subst v e b )
    subst v e (Pi  v' ta tb) | v == v' = Pi  v' (subst v e ta)            tb
    subst v e (Pi  v' ta tb)           = Pi  v' (subst v e ta) (subst v e tb)
    subst v e (App f a     )           = App    (subst v e f ) (subst v e a )
    subst v e e' = e'

    -- Deduce if a variable v is free in an expression e
    free :: Int -> Expr -> Bool
    free v e = e /= subst v (Var $ v + 1) e

    -- Reduce an expression to its normal form, performing both beta reduction and
    -- eta reduction
    -- `normalize` does not type-check the expression.  You may want to type-check
    -- expressions before normalizing them since normalization can convert an
    -- ill-typed expression into a well-typed expression.
    normalize :: Expr -> Expr
    normalize (Lam v ta b) = case normalize b of
        App vb (Var v') | v == v' && not (free v vb) -> vb -- Eta reduce
        b' -> Lam v (normalize ta) b'
    normalize (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
    normalize (App f a) = case normalize f of
        Lam v _ b -> subst v (normalize a) b -- Beta reduce
        f' -> App f' (normalize a)
    normalize c = c

    -- Deduce if e is an equivalent expression to e'
    equiv :: Expr -> Expr -> Bool
    e `equiv` e' = equiv' (normalize e) (normalize e') (-1)

    equiv' :: Expr -> Expr -> Int -> Bool
    equiv' (Lam v ta b) (Lam v' ta' b') n = equiv' ta ta' n && equiv' (subst v (Var n)  b) (subst v' (Var n)  b') (pred n)
    equiv' (Pi v ta tb) (Pi v' ta' tb') n = equiv' ta ta' n && equiv' (subst v (Var n) tb) (subst v' (Var n) tb') (pred n)
    equiv' (App f a) (App f' a') n = equiv' f f' n && equiv' a a' n
    equiv' c c' n = c == c'

    -- Type-check an expression and return the expression's type if type-checking
    -- suceeds or Nothing if type-checking fails
    -- `typeIn` does not necessarily normalize the type since full normalization
    -- is not necessary for just type-checking.  If you actually care about the
    -- returned type then you may want to `normalize` it afterwards.
    typeIn :: [(Int, Expr)] -> Expr -> Maybe Expr
    typeIn _ Star = return Box
    typeIn _ Box  = Nothing
    typeIn ctx (Var v) = lookup v ctx
    typeIn ctx (Lam v ta b) = do
        tb <- typeIn ((v, ta):ctx) b
        let tf = Pi v ta tb
        _ttf <- typeIn ctx tf
        return tf
    typeIn ctx (Pi v ta tb) = do
        tta <- typeIn ctx ta
        ttb <- typeIn ((v, ta):ctx) tb
        case (tta, ttb) of
            (Star, Star) -> return Star
            (Box , Star) -> return Star
            (Star, Box ) -> return Box
            (Box , Box ) -> return Box
            _            -> Nothing
    typeIn ctx (App f a) = do
        (v, ta, tb) <- case typeIn ctx f of
            Just (Pi v ta tb) -> return (v, ta, tb)
            _                 -> Nothing
        ta' <- typeIn ctx a
        if ta `equiv` ta'
        then return $ subst v a tb
        else Nothing

    -- `typeOf` is the same as `typeIn` with an empty context, meaning that the
    -- expression must be closed (i.e. no free variables), otherwise type-checking
    -- will fail.
    typeOf :: Expr -> Maybe Expr
    typeOf = typeIn []

    -- Deduce if an expression e is well-typed
    wellTyped :: Expr -> Bool
    wellTyped e = typeOf e /= Nothing


    -- test example

    test :: IO ()
    test = do
        putStrLn $ "Constructions Test Suite"
        putStrLn $ "------------------------"

        -- let id'     = Lam (Inf (Bound 0))
        -- let const'  = Lam (Lam (Inf (Bound 1)))
        -- let tFree a = TFree (Global a)
        -- let free  x = Inf (Free (Global x))

        -- -- >> assume (a :: *) (y :: a)
        -- let env1 = [(Global "y", HasType (tFree "a")), (Global "a", HasKind Star)]
        -- -- >> (id :: a -> a) y
        -- let term1 = Ann id' (Fun (tFree "a") (tFree "a")) `App` free "y"
        -- -- ~> y :: a
        -- let eval1 = free "y"
        -- let type1 = TFree (Global "a")
        -- -- assert
        -- putStrLn $ "term: " ++ show term1
        -- putStrLn $ "eval: " ++ show (assertEquals (quote0 (infEval term1 [])) eval1)
        -- putStrLn $ "env:  " ++ showCtx env1
        -- case infType0 env1 term1 of
        --     Left err  -> error err
        --     Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type1)
        -- putStrLn ""

        return ()
