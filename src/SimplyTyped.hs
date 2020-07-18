module SimplyTyped where

    import Data.List (intersperse)
    import Util

    data InfTerm
        = Ann ChkTerm Type
        | Bound Int
        | Free Name
        | App InfTerm ChkTerm
        deriving (Eq)
    instance Show InfTerm where
        show = showITerm0

    data ChkTerm
        = Inf InfTerm
        | Lam ChkTerm
        deriving (Eq)
    instance Show ChkTerm where
        show = showCTerm0

    data Name
        = Global String
        | Local Int
        | Quote Int
        deriving (Eq)
    instance Show Name where
        show (Global name) = name
        show (Local int)   = show int
        show (Quote int)   = show int

    data Type
        = TFree Name
        | Fun Type Type
        deriving (Eq)
    instance Show Type where
        show (TFree name)     = show name
        show (Fun left right) = "(" ++ show left ++ " -> " ++ show right ++ ")"

    data Value
        = VLam (Value -> Value)
        | VNeutral Neutral

    data Neutral
        = NFree Name
        | NApp Neutral Value

    data Kind
        = Star
    instance Show Kind where
        show Star = show "*"

    data Info
        = HasKind Kind
        | HasType Type
    instance Show Info where
        show (HasKind k) = show k
        show (HasType τ) = show τ

    type Env = [Value]

    type Context = [(Name, Info)]


    -- pretty-printing for terms --

    showCTerm0 :: ChkTerm -> String
    showCTerm0 ρ = str
        where (str, _) = showCTerm ρ 0

    showCTerm :: ChkTerm -> Int -> (String, Int)
    showCTerm (Inf inf) i = showITerm inf i
    showCTerm (Lam exp) i = ("λ " ++ show j ++ " . " ++ e, j + 1)
        where (e, j) = showCTerm exp i

    showITerm0 :: InfTerm -> String
    showITerm0 e = str
        where (str, _) = showITerm e 0

    showITerm :: InfTerm -> Int -> (String, Int)
    showITerm (Ann e ρ) i  = (show e ++ " : " ++ show ρ, i)
    showITerm (Bound j) i  = (show (j + i), i)
    showITerm (Free x) i   = (show x, i)
    showITerm (App ρ ρ') i = ("(" ++ e ++ ") (" ++ e' ++ ")", j')
        where (e, j)   = showITerm ρ i
              (e', j') = showCTerm ρ' j

    showCtx :: Context -> String
    showCtx ctx = concat $ intersperse ", " (map (\(n, τ) -> show n ++ " : " ++ show τ) ctx)


    -- type checking and type inference

    vFree :: Name -> Value
    vFree n = VNeutral (NFree n)

    vApp :: Value -> Value -> Value
    vApp (VLam f) v = f v
    vApp (VNeutral n) v = VNeutral (NApp n v)

    infEval :: InfTerm -> Env -> Value
    infEval (Ann e _) d = chkEval e d
    infEval (Free x)  _ = vFree x
    infEval (Bound i) d = d !! i
    infEval (App e e') d = vApp (infEval e d) (chkEval e' d)

    chkEval :: ChkTerm -> Env -> Value
    chkEval (Inf i) d = infEval i d
    chkEval (Lam e) d = VLam (\x -> chkEval e (x:d))

    chkKind :: Context -> Type -> Kind -> Result ()
    chkKind γ (TFree x) Star = case lookup x γ of
        Just (_) -> return ()
        Nothing  -> throwError "unknown identifier"
    chkKind γ (Fun k k') Star = do
        chkKind γ k Star
        chkKind γ k' Star

    infType0 :: Context -> InfTerm -> Result Type
    infType0 = infType 0

    infType :: Int -> Context -> InfTerm -> Result Type
    infType i γ (Ann e τ) = do
        chkKind γ τ Star
        chkType i γ e τ
        return τ
    infType _ γ (Free x) = case lookup x γ of
        Just (HasType τ) -> return τ
        Just (HasKind _) -> throwError "encountered kind instead of type"
        Nothing          -> throwError "unknown identifier"
    infType i γ (App e e') = do
        σ <- infType i γ e
        case σ of
            Fun τ τ' -> do
                chkType i γ e' τ
                return τ'
            _        -> throwError "illegal application"
    infType _ _ (Bound _) = throwError "illegal application"

    chkType :: Int -> Context -> ChkTerm -> Type -> Result ()
    chkType i γ (Inf e) τ = do
        τ' <- infType i γ e
        if (τ == τ')
            then Right ()
            else (throwError "type mismatch")
    chkType i γ (Lam e) (Fun τ τ') =
        chkType (i + 1) ((Local i, HasType τ):γ) (chkSubst 0 (Free(Local i)) e) τ'
    chkType _ _ _ _ = throwError "type mismatch"

    infSubst :: Int -> InfTerm -> InfTerm -> InfTerm
    infSubst i r (Ann e τ)  = Ann (chkSubst i r e) τ
    infSubst i r (Bound j)  = if i == j then r else Bound j
    infSubst i r (App e e') = infSubst i r (App e (chkSubst i r e'))
    infSubst _ _ (Free y)   = Free y

    chkSubst :: Int -> InfTerm -> ChkTerm -> ChkTerm
    chkSubst i r (Inf e) = Inf (infSubst i r e)
    chkSubst i r (Lam e) = Lam (chkSubst (i + 1) r e)

    quote0 :: Value -> ChkTerm
    quote0 = quote 0

    quote :: Int -> Value -> ChkTerm
    quote i (VLam f)     = Lam (quote (i + 1) (f (vFree (Quote i))))
    quote i (VNeutral n) = Inf (neutralQuote i n)

    neutralQuote:: Int -> Neutral -> InfTerm
    neutralQuote i (NFree x)  = boundFree i x
    neutralQuote i (NApp n v) = App (neutralQuote i n) (quote i v)

    boundFree :: Int -> Name -> InfTerm
    boundFree i (Quote k) = Bound (i - k - 1)
    boundFree _ x         = Free x


    -- test example

    test :: IO ()
    test = do
        putStrLn $ "SimplyTyped Test Suite"
        putStrLn $ "----------------------"

        let id'     = Lam (Inf (Bound 0))
        let const'  = Lam (Lam (Inf (Bound 1)))
        let tFree a = TFree (Global a)
        let free  x = Inf (Free (Global x))

        -- >> assume (a :: *) (y :: a)
        let env1 = [(Global "y", HasType (tFree "a")), (Global "a", HasKind Star)]
        -- >> (id :: a -> a) y
        let term1 = Ann id' (Fun (tFree "a") (tFree "a")) `App` free "y"
        -- ~> y :: a
        let eval1 = free "y"
        let type1 = TFree (Global "a")
        -- assert
        putStrLn $ "term: " ++ show term1
        putStrLn $ "eval: " ++ show (assertEquals (quote0 (infEval term1 [])) eval1)
        putStrLn $ "env:  " ++ showCtx env1
        case infType0 env1 term1 of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type1)
        putStrLn ""

        -- >> assume (b :: *)
        let env2 = [(Global "b", HasKind Star)] ++ env1
        -- >> (const :: (b -> b) -> a -> b -> b) id y
        let term2 = Ann const' (Fun (Fun (tFree "b") (tFree "b")) (Fun (tFree "a") (Fun (tFree "b") (tFree "b")))) `App` id' `App` free "y"
        -- ~> id :: b -> b
        let eval2 = id'
        let type2 = Fun (TFree (Global "b")) (TFree (Global "b"))
        -- assert
        putStrLn $ "term: " ++ show term2
        putStrLn $ "eval: " ++ show (assertEquals (quote0 (infEval term2 [])) eval2)
        putStrLn $ "env:  " ++ showCtx env2
        case infType0 env2 term2 of
            Left err  -> error err
            Right inf -> putStrLn $ "type: " ++ show (assertEquals inf type2)
        putStrLn ""

        return ()
