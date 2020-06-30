module SimplyTyped where

    data InfTerm
        = Ann ChkTerm Type
        | Bound Int
        | Free Name
        | App InfTerm ChkTerm
        deriving (Show, Eq)

    data ChkTerm
        = Inf InfTerm
        | Lam ChkTerm
        deriving (Show, Eq)

    data Name
        = Global String
        | Local Int
        | Quote Int
        deriving (Show, Eq)

    data Type
        = TFree Name
        | Fun Type Type
        deriving (Show, Eq)

    data Value
        = VLam (Value -> Value)
        | VNeutral Neutral

    data Neutral
        = NFree Name
        | NApp Neutral Value

    vFree :: Name -> Value
    vFree n = VNeutral (NFree n)

    type Env = [Value]

    infEval :: InfTerm -> Env -> Value
    infEval (Ann e _) d = chkEval e d
    infEval (Free x)  _ = vFree x
    infEval (Bound i) d = d !! i
    infEval (App e e') d = vApp (infEval e d) (chkEval e' d)

    vApp :: Value -> Value -> Value
    vApp (VLam f) v = f v
    vApp (VNeutral n) v = VNeutral (NApp n v)

    chkEval :: ChkTerm -> Env -> Value
    chkEval (Inf i) d = infEval i d
    chkEval (Lam e) d = VLam (\x -> chkEval e (x:d))

    data Kind
        = Star
        deriving (Show)

    data Info
        = HasKind Kind
        | HasType Type
        deriving (Show)

    type Context = [(Name, Info)]

    type Result a = Either String a

    throwError :: String -> Result a
    throwError = error

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

    -- poor man's unittest lib --
    assertEquals :: (Show a, Eq a) => a -> a -> a
    assertEquals x y = case x == y of
        True  -> x
        False -> error $ (show x) ++ " != " ++ (show y)

    main :: IO ()
    main = do
        putStrLn $ "SimplyTyped Test Suite"

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
        let type1 = Right (TFree (Global "a"))
        -- assert
        print $ assertEquals (quote0 (infEval term1 [])) eval1
        print $ assertEquals (infType0 env1 term1) type1

        -- >> assume (b :: *)
        let env2 = [(Global "b", HasKind Star)] ++ env1
        -- >> (const :: (b -> b) -> a -> b -> b) id y
        let term2 = Ann const' (Fun (Fun (tFree "b") (tFree "b")) (Fun (tFree "a") (Fun (tFree "b") (tFree "b")))) `App` id' `App` free "y"
        -- ~> id :: b -> b
        let eval2 = id'
        let type2 = Right (Fun (TFree (Global "b")) (TFree (Global "b")))
        -- assert
        print $ assertEquals (quote0 $ infEval term2 []) eval2
        print $ assertEquals (infType0 env2 term2) type2

        putStrLn ""
        return ()
