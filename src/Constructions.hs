module Constructions where

    import Data.List
    import Util

    data Expr a = Star
              | Box
              | Var a
              | Lam a (Expr a) (Expr a)
              | Pi a (Expr a) (Expr a)
              | App (Expr a) (Expr a)
              deriving (Eq)
    instance Show a => Show (Expr a) where
        show = showExpr

    type PartialContext a b = [(a, Expr b)]
    type Context a = PartialContext a a

    type DeBruijnExpr = Expr Int
    type DeBruijnCtx = Context Int


    -- pretty-printing for expressions

    showExpr :: Show a => Expr a -> String
    showExpr (Star)      = "*"
    showExpr (Box)       = "□"
    showExpr (Var i)     = show i
    showExpr (Lam i τ e) = "λ (" ++ show i ++ " : " ++ show τ ++ ") . " ++ show e
    showExpr (Pi i τ e)  = "π (" ++ show i ++ " : " ++ show τ ++ ") . " ++ show e
    showExpr (App e ρ)   = "(" ++ show e ++ ") (" ++ show ρ ++ ")"

    showCtx :: Show a => Context a -> String
    showCtx ctx = concat $ intersperse ", " (map (\(n, τ) -> show n ++ " : " ++ show τ) ctx)


    -- named variable substitution

    index :: (Eq a, Show a) => Expr a -> Context a -> (DeBruijnExpr, DeBruijnCtx)
    index e γ = (idxExpr (zip (map fst γ) ctx) [] e, zip (map negate [1..]) ctx)
        where ctx = map (idxCtx (map fst γ) []) (map snd γ)

    idxCtx :: (Eq a, Show a) => [a] -> [a] -> Expr a -> DeBruijnExpr
    idxCtx = idx idxCtx0 length
        where idxCtx0 γ σ v = (v `elemIndex` γ, v `elemIndex` σ)

    idxExpr :: (Eq a, Show a) => PartialContext a Int -> [a] -> Expr a -> DeBruijnExpr
    idxExpr = idx idxExpr0 length
        where idxExpr0 γ σ v = (((==) v . fst) `findIndex` γ, v `elemIndex` σ)

    idx :: (Show a) => ([b] -> [a] -> a -> (Maybe Int, Maybe Int)) -> ([a] -> Int) -> [b] -> [a] -> Expr a -> DeBruijnExpr
    idx φ θ γ = idx0
        where idx0 _ Star = Star
              idx0 _ Box  = Box
              idx0 σ (Var v) = case (φ γ σ v) of
                (Just idx, Nothing)  -> Var $ -1 - idx
                (Nothing,  Just idx) -> Var $ idx
                _                    -> error $ "not in scope: " ++ show v
              idx0 σ (Lam a τ e) = Lam (θ σ) (idx0 σ τ) (idx0 (σ ++ [a]) e)
              idx0 σ (Pi a τ e)  = Pi (θ σ) (idx0 σ τ) (idx0 (σ ++ [a]) e)
              idx0 σ (App e ρ)   = App (idx0 σ e) (idx0 σ ρ) 

    named :: DeBruijnExpr -> DeBruijnCtx -> Context a -> Expr a
    named = undefined


    -- type checking and type inference

    -- Substitute all occurrences of a variable v with an expression e
    -- subst x n C B  ~  B[x@n := C]
    subst :: Int -> DeBruijnExpr -> DeBruijnExpr -> DeBruijnExpr
    subst v e (Var v')       | v == v' = e
    subst v e (Lam v' ta b ) | v == v' = Lam v' (subst v e ta)            b
    subst v e (Lam v' ta b )           = Lam v' (subst v e ta) (subst v e b )
    subst v e (Pi  v' ta tb) | v == v' = Pi  v' (subst v e ta)            tb
    subst v e (Pi  v' ta tb)           = Pi  v' (subst v e ta) (subst v e tb)
    subst v e (App f a     )           = App    (subst v e f ) (subst v e a )
    subst v e e'                       = e'

    -- Deduce if a variable v is free in an expression e
    free :: Int -> DeBruijnExpr -> Bool
    free v e = e /= subst v (Var $ v + 1) e

    -- Reduce an expression to its normal form, performing both beta reduction and
    -- eta reduction
    -- `normalize` does not type-check the expression.  You may want to type-check
    -- expressions before normalizing them since normalization can convert an
    -- ill-typed expression into a well-typed expression.
    normalize :: DeBruijnExpr -> DeBruijnExpr
    normalize (Lam v ta b) = case normalize b of
        App vb (Var v') | v == v' && not (free v vb) -> vb -- Eta reduce
        b'                                           -> Lam v (normalize ta) b'
    normalize (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
    normalize (App f a) = case normalize f of
        Lam v _ b -> subst v (normalize a) b -- Beta reduce
        f'        -> App f' (normalize a)
    normalize c = c

    -- Deduce if e is an equivalent expression to e'
    equiv :: DeBruijnExpr -> DeBruijnExpr -> Bool
    e `equiv` e' = equiv' (normalize e) (normalize e') (-1)

    equiv' :: DeBruijnExpr -> DeBruijnExpr -> Int -> Bool
    equiv' (Lam v ta b) (Lam v' ta' b') n = equiv' ta ta' n && equiv' (subst v (Var n)  b) (subst v' (Var n)  b') (pred n)
    equiv' (Pi v ta tb) (Pi v' ta' tb') n = equiv' ta ta' n && equiv' (subst v (Var n) tb) (subst v' (Var n) tb') (pred n)
    equiv' (App f a)    (App f' a')     n = equiv' f f' n && equiv' a a' n
    equiv' c            c'              n = c == c'

    -- Type-check an expression and return the expression's type if type-checking
    -- suceeds or Nothing if type-checking fails
    -- `typeIn` does not necessarily normalize the type since full normalization
    -- is not necessary for just type-checking.  If you actually care about the
    -- returned type then you may want to `normalize` it afterwards.
    typeIn :: DeBruijnCtx -> DeBruijnExpr -> Result DeBruijnExpr
    typeIn _ Star = return Box
    typeIn _ Box  = throwError "absurd box"
    typeIn ctx (Var v) = case lookup v ctx of
        Nothing -> throwError $ "unbound variable: " ++ show v
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
    typeOf :: DeBruijnExpr -> Result DeBruijnExpr
    typeOf = typeIn []

    -- Deduce if an expression e is well-typed
    wellTyped :: DeBruijnExpr -> Bool
    wellTyped e = case typeOf e of
        Right _ -> True
        Left _  -> False


    -- test example

    test :: IO ()
    test = do
        putStrLn $ "Constructions Test Suite"
        putStrLn $ "------------------------"

        -- id = λ x:* -> λ y:x -> y :: π x:* -> π y:x -> x
        let term1' = Lam "x" Star (Lam "y" (Var "x") (Var "y"))
        -- >> assume (Bool :: *") (False :: Bool)
        let ctx' = [("Bool", Star), ("False", Var ("Bool"))]

        -- 'compile'
        let (term1, ctx) = index term1' ctx'
        let eval1 = term1
        let type1 = Pi 0 Star (Pi 1 (Var 0) (Var 0))
        -- assert
        putStrLn $ "term: " ++ show term1'
        putStrLn $ "ctx:  " ++ showCtx ctx'
        putStrLn $ "eval: " ++ show (assertEquals (normalize term1) eval1)
        -- putStrLn $ "ctx:  " ++ showCtx ctx
        case (typeOf $ normalize term1) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type1)
        putStrLn ""
        
        -- id Bool = λ y:Bool -> y :: π y:Bool -> Bool
        let term2' = term1' `App` Var "Bool"

        -- 'compile'
        let (term2, ctx) = index term2' ctx'
        let eval2 = normalize term2
        let type2 = either (\x -> Box) id (typeIn ctx eval2)
        -- assert
        putStrLn $ "term: " ++ show term2'
        putStrLn $ "ctx:  " ++ showCtx ctx'
        putStrLn $ "eval: " ++ show (assertEquals (normalize term2) eval2)
        case (typeIn ctx $ normalize term2) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type2)
        putStrLn ""

        -- id Bool False = False :: Bool
        let term3' = term2' `App` Var "False"

        -- 'compile'
        let (term3, ctx) = index term3' ctx'
        let eval3 = normalize term3
        let type3 = either (\x -> Box) id (typeIn ctx eval3)
        -- assert
        putStrLn $ "term: " ++ show term3'
        putStrLn $ "ctx:  " ++ showCtx ctx'
        putStrLn $ "eval: " ++ show (assertEquals (normalize term3) eval3)
        case (typeIn ctx $ normalize term3) of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type3)
        putStrLn ""

        return ()
