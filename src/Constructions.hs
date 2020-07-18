module Constructions where

    import Data.List (intersperse)
    import Util

    data Expr = Star
              | Box
              | Var Int
              | Lam Int Expr Expr
              | Pi Int Expr Expr
              | App Expr Expr
              deriving (Eq)
    instance Show Expr where
        show = showExpr

    type Context = [(Int, Expr)]

    -- pretty-printing for expressions

    showExpr :: Expr -> String
    showExpr (Star)       = "*"
    showExpr (Box)        = "□"
    showExpr (Var i)      = show i
    showExpr (Lam i e e') = "λ (" ++ show i ++ " : " ++ show e ++ ") . " ++ show e'
    showExpr (Pi i e e')  = "π (" ++ show i ++ " : " ++ show e ++ ") . " ++ show e'
    showExpr (App e ρ)    = "( " ++ show e ++ " ) ( " ++ show ρ ++ " )"

    showCtx :: Context -> String
    showCtx ctx = concat $ intersperse ", " (map (\(n, τ) -> show n ++ " : " ++ show τ) ctx)


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
    typeIn :: Context -> Expr -> Result Expr
    typeIn _ Star = return Box
    typeIn _ Box  = throwError "absurd box?"
    typeIn ctx (Var v) = case lookup v ctx of
        Nothing -> throwError $ "unbound variable: " ++ show (Var v)
        Just e  -> Right e
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
            _            -> throwError $ "invalid type: " ++ show (Pi v ta tb) 
    typeIn ctx (App f a) = do
        (v, ta, tb) <- case typeIn ctx f of
            Right (Pi v ta tb) -> return (v, ta, tb)
            _                  -> throwError $ "not a function: " ++ show (App f a)
        ta' <- typeIn ctx a
        if ta `equiv` ta'
        then return $ subst v a tb
        else throwError $ "type mismatch: " ++ show ta ++ " != " ++ show ta'

    -- `typeOf` is the same as `typeIn` with an empty context, meaning that the
    -- expression must be closed (i.e. no free variables), otherwise type-checking
    -- will fail.
    typeOf :: Expr -> Result Expr
    typeOf = typeIn []

    -- Deduce if an expression e is well-typed
    wellTyped :: Expr -> Bool
    wellTyped e = case typeOf e of
        Right _ -> True
        Left _  -> False


    -- test example

    test :: IO ()
    test = do
        putStrLn $ "Constructions Test Suite"
        putStrLn $ "------------------------"

        -- -- >> id = λx -> λy -> y
        -- let id'     = Lam (Lam (Inf (Bound 0)))
        -- let tFree a = vFree (Global a)
        -- let free  x = Inf (Free (Global x))

        -- let t' = Inf (Pi (Inf Star) (Inf (Pi (Inf $ Bound 0) (Inf $ Bound 1))))
        -- let term1 = id' `Ann` t'
        -- -- ~> λx -> λy -> y :: πx::* -> πy::x -> y
        -- let eval1 = quote0 $ infEval term1 []
        -- -- assert
        -- putStrLn $ "term: " ++ show term1
        -- putStrLn $ "eval: " ++ show eval1
        -- putStrLn $ "env:  " ++ showCtx ([] :: Context)
        -- case infType0 [] term1 of
        --     Left err  -> error err
        --     Right inf -> putStrLn $ "type: " ++ show (quote0 inf)
        -- putStrLn ""

        -- id = λ x:* -> λ y:x -> y
        let term1 = Lam 0 Star (Lam 1 (Var 0) (Var 1)) 
        -- ~> id :: (πx::* -> πy::x -> x)
        let eval1 = term1
        let type1 = Pi 0 Star (Pi 1 (Var 0) (Var 0))
        -- assert
        putStrLn $ "term: " ++ show term1
        putStrLn $ "ctx:  " ++ showCtx ([] :: Context)
        putStrLn $ "eval: " ++ show (assertEquals (normalize term1) eval1)
        -- putStrLn $ "ctx:  " ++ showCtx ctx
        case (typeOf $ normalize term1) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type1)
        putStrLn ""

        -- >> assume (Bool :: *) (False :: Bool)
        let ctx = [(-1, Star), (-2, Var (-1))]

        -- id Bool = -> λ y:Bool -> y
        let term2 = term1 `App` Var (-1)
        -- ~> id Bool :: (πy::Bool -> Bool)
        let eval2 = normalize term2
        let type2 = either (\x -> Box) id (typeIn ctx eval2)
        -- assert
        putStrLn $ "term: " ++ show term2
        putStrLn $ "ctx:  " ++ showCtx ctx
        putStrLn $ "eval: " ++ show (assertEquals (normalize term2) eval2)
        case (typeIn ctx $ normalize term2) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type2)
        putStrLn ""

        -- id Bool False = -> False
        let term3 = term2 `App` Var (-2)
        -- ~> id Bool False :: Bool
        let eval2 = normalize term2
        let type2 = either (\x -> Box) id (typeIn ctx eval2)
        -- assert
        putStrLn $ "term: " ++ show term2
        putStrLn $ "ctx:  " ++ showCtx ctx
        putStrLn $ "eval: " ++ show (assertEquals (normalize term2) eval2)
        case (typeIn ctx $ normalize term2) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type2)
        putStrLn ""

        return ()
