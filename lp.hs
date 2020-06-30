data InfTerm
    = Ann ChkTerm ChkTerm
    | Star
    | Pi ChkTerm ChkTerm
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
    = VLam (Type -> Type)
    | VStar
    | VPi Type (Type -> Type)
    | VNeutral Neutral

data Neutral
    = NFree Name
    | NApp Neutral Type

vFree :: Name -> Type
vFree n = VNeutral (NFree n)

type Env = [Type]

infEval :: InfTerm -> Env -> Type
infEval (Ann e _) d = chkEval e d
infEval Star      d = VStar
infEval (Pi τ τ') d = VPi (chkEval τ d) (\x -> chkEval τ' (x:d))
infEval (Free x)  _ = vFree x
infEval (Bound i) d = d !! i
infEval (App e e') d = vApp (infEval e d) (chkEval e' d)

vApp :: Type -> Type -> Type
vApp (VLam f) v = f v
vApp (VNeutral n) v = VNeutral (NApp n v)

chkEval :: ChkTerm -> Env -> Type
chkEval (Inf i) d = infEval i d
chkEval (Lam e) d = VLam (\x -> chkEval e (x:d))

type Context = [(Name, Type)]

type Result a = Either String a

throwError :: String -> Result a
throwError = error

infType0 :: Context -> InfTerm -> Result Type
infType0 = infType 0

infType :: Int -> Context -> InfTerm -> Result Type
infType i γ (Ann e ρ) = do
    let τ = chkEval ρ []
    chkType i γ e τ
    return τ
infType i γ Star = do
    return VStar
infType i γ (Pi ρ ρ') = do
    chkType i γ ρ VStar
    let τ = chkEval ρ []
    chkType (i + 1) ((Local i, τ):γ) (chkSubst 0 (Free (Local i)) ρ') VStar
    return VStar
infType _ γ (Free x) = case lookup x γ of
    Just τ  -> return τ
    Nothing -> throwError "unknown identifier"
infType i γ (App e e') = do
    σ <- infType i γ e
    case σ of
        VPi τ τ' -> do
            chkType i γ e' τ
            return (τ' (chkEval e' []))
        _        -> throwError "illegal application"
infType _ _ (Bound _) = throwError "illegal application"

chkType :: Int -> Context -> ChkTerm -> Type -> Result ()
chkType i γ (Inf e) v = do
    v' <- infType i γ e
    if (quote0 v == quote0 v')
        then Right ()
        else (throwError "type mismatch")
chkType i γ (Lam e) (VPi τ τ') =
    chkType (i + 1) ((Local i, τ):γ) (chkSubst 0 (Free(Local i)) e) (τ' (vFree (Local i)))
chkType _ _ _ _ = throwError "type mismatch"

infSubst :: Int -> InfTerm -> InfTerm -> InfTerm
infSubst i r (Ann e τ)  = Ann (chkSubst i r e) (chkSubst i r τ)
infSubst i r Star       = Star
infSubst i r (Pi τ τ')  = Pi (chkSubst i r τ) (chkSubst (i + 1) r τ')
infSubst i r (Bound j)  = if i == j then r else Bound j
infSubst i r (App e e') = infSubst i r (App e (chkSubst i r e'))
infSubst _ _ (Free y)   = Free y

chkSubst :: Int -> InfTerm -> ChkTerm -> ChkTerm
chkSubst i r (Inf e) = Inf (infSubst i r e)
chkSubst i r (Lam e) = Lam (chkSubst (i + 1) r e)

quote0 :: Type -> ChkTerm
quote0 = quote 0

quote :: Int -> Type -> ChkTerm
quote i (VLam f)     = Lam (quote (i + 1) (f (vFree (Quote i))))
quote i VStar        = Inf Star
quote i (VPi v f)    = Inf (Pi (quote i v) (quote (i + 1) (f (vFree (Quote i)))))
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
    let id'     = Lam (Lam (Inf (Bound 0)))
    let const'  = Lam (Lam (Inf (Bound 1)))
    let tFree a = vFree (Global a)
    let free  x = Inf (Free (Global x))

    -- >> id :: Πx::* . Πy::x . y
    let term1 = id' `Ann` (Inf (Pi (Inf Star) (Inf (Pi (Inf (Bound 0)) (Inf (Bound 1))))))
    -- ~> λx -> λy -> y :: Πx::* -> Πy::x -> y
    let eval1 = quote0 $ infEval term1 []
    -- assert
    print $ eval1
    case infType0 [] term1 of
        Left err  -> error err
        Right inf -> print $ quote0 inf

    -- >> assume (Bool :: *) (False :: Bool)
    let env = [(Global "False", tFree "Bool"), (Global "Bool", VStar)]
    -- >> id Bool
    let term2 = term1 `App` (free "Bool")
    -- ~> λx -> x :: Πx::Bool -> Bool
    let eval2 = Lam (Inf (Bound 0))
    let type2 = Inf (Pi (free "Bool") (free "Bool"))
    -- assert
    print $ assertEquals (quote0 $ infEval term2 []) eval2
    case infType0 env term2 of
        Left err  -> error err
        Right inf -> print $ assertEquals (quote0 inf) type2

    -- >> id Bool False
    let term3 = term2 `App` (free "False")
    -- ~> λx -> x :: Πx::Bool -> Bool
    let eval3 = free "False"
    let type3 = free "Bool"
    -- assert
    print $ assertEquals (quote0 $ infEval term3 []) eval3
    case infType0 env term3 of
        Left err  -> error err
        Right inf -> print $ assertEquals (quote0 inf) type3

    return ()
