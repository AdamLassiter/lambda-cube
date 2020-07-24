module Pretty where
    import Core
    import Util

    import Data.List
    import Data.Either

    type NamedExpr = Expr String
    type NamedCtx = Context String


    -- pretty-printing for expressions

    showExpr :: (a -> String) -> Expr a -> String
    showExpr _ (Star)      = "*"
    showExpr _ (Box)       = "#"
    showExpr s (Var i)     = s i
    showExpr s (Lam i τ e) = "λ " ++ s i ++ " : " ++ showExpr s τ ++ " . " ++ showExpr s e
    showExpr s (Pi i τ e)  = "π " ++ s i ++ " : " ++ showExpr s τ ++ " . " ++ showExpr s e
    showExpr s (App e ρ)   = "(" ++ showExpr s e ++ ") (" ++ showExpr s ρ ++ ")"

    showCtx :: (a -> String) -> Context a -> String
    showCtx s ctx = concat $ intersperse ", " (map (\(n, τ) -> s n ++ " : " ++ showExpr s τ) ctx)


    -- named variable substitution

    index :: (Eq a, Show a) => Expr a -> Context a -> Result (DeBruijnExpr, DeBruijnCtx)
    index e γ = if errs == []
        then mapR (\e' -> (e', ctx'')) expr
        else throwError $ head errs 
        where ctx = map (idxCtx (map fst γ) []) (map snd γ)
              (errs, ctx') = partitionEithers ctx
              (expr, ctx'') = (idxExpr (zip (map fst γ) ctx') [] e, zip (map negate [1..]) ctx')

    idxCtx :: (Eq a, Show a) => [a] -> [a] -> Expr a -> Result DeBruijnExpr
    idxCtx = idx idxCtx0 length
        where idxCtx0 γ s v = (v `elemIndex` γ, v `elemIndex` s)

    idxExpr :: (Eq a, Show a) => PartialContext a Int -> [a] -> Expr a -> Result DeBruijnExpr
    idxExpr = idx idxExpr0 length
        where idxExpr0 γ s v = (((==) v . fst) `findIndex` γ, v `elemIndex` s)

    idx :: (Show a) => ([b] -> [a] -> a -> (Maybe Int, Maybe Int)) -> ([a] -> Int) -> [b] -> [a] -> Expr a -> Result DeBruijnExpr
    idx φ θ γ = idx0
        where idx0 _ Star = Right $ Star
              idx0 _ Box  = Right $ Box
              idx0 s (Var v) = case (φ γ s v) of
                (_,        Just idx) -> Right $ Var $ idx
                (Just idx, Nothing)  -> Right $ Var $ -1 - idx
                (Nothing, Nothing)   -> throwError $ "not in scope: " ++ show v
              idx0 s (Lam a τ e) = fmapR (\τ' -> mapR (\e' -> Lam (θ s) τ' e') (idx0 (s ++ [a]) e)) (idx0 s τ)
              idx0 s (Pi a τ e)  = fmapR (\τ' -> mapR (\e' -> Pi (θ s) τ' e') (idx0 (s ++ [a]) e)) (idx0 s τ)
              idx0 s (App e ρ)   = fmapR (\e' -> mapR (\ρ' -> App e' ρ') (idx0 s ρ)) (idx0 s e)

    named :: DeBruijnExpr -> DeBruijnCtx -> NamedCtx -> NamedExpr
    named e γ γ' = quote e γ γ' (map show [0..])

    quote :: (Eq a) => DeBruijnExpr -> DeBruijnCtx -> Context a -> [a] -> Expr a
    quote e γ γ' s = quote0 e
        where quote0 Star = Star
              quote0 Box  = Box
              quote0 (Var v) = case (((==) v . fst) `find` γ) of
                  Just (i, _) -> Var (fst $ γ' !! (-1 - i))
                  Nothing     -> Var (s !! v)
              quote0 (Lam a τ e) = Lam (s !! a) (quote0 τ) (quote0 e)
              quote0 (Pi a τ e)  = Pi (s !! a) (quote0 τ) (quote0 e)
              quote0 (App e ρ)   = App (quote0 e) (quote0 ρ)


    -- core functions wrapped in namedexprs

    -- normalize a named expression
    nNormalizeOf :: NamedExpr -> NamedCtx -> Result NamedExpr
    nNormalizeOf expr τCtx = case index expr τCtx of
        Right (dbExpr, dbCtx) -> Right eval
            where dbEval = normalize dbExpr
                  eval = named dbEval dbCtx τCtx 
        Left err -> Left err

    -- `normalizeIn` is the same as `normalizeOf` but in a context binding free names
    -- to expressions.
    nNormalizeIn :: NamedCtx -> NamedExpr -> NamedCtx -> Result NamedExpr
    nNormalizeIn eCtx expr = nNormalizeOf (foldl (\expr (i, e) -> subst i e expr) expr eCtx)

    -- type a named expression
    nTypeOf :: NamedExpr -> NamedCtx -> Result NamedExpr
    nTypeOf expr τCtx = case index expr τCtx of
        Right (dbExpr, dbCtx) -> case typeIn dbCtx dbExpr of
            Right dbType -> Right type'
                where type' = named dbType dbCtx τCtx 
            Left err -> Left err
        Left err -> Left err

    -- `typeIn` is the same as `typeOf` but in a context binding free names
    -- to expressions.
    nTypeIn :: NamedCtx -> NamedExpr -> NamedCtx -> Result NamedExpr
    nTypeIn eCtx expr = nTypeOf (foldl (\expr (i, e) -> subst i e expr) expr eCtx)
