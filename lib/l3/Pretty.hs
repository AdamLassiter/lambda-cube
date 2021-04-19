-- Pretty-printing for expressions (ie. with Strings instead of Ints)
module L3.Pretty (module L3.Pretty, module L3.Core) where
    import L3.Core
    import L3.Util

    import Data.List
    import Data.Either

    type ShowExpr = Expr String
    type ShowCtx = Context String

    -- Show an expression using a naming function
    showExpr :: (a -> String) -> Expr a -> String
    showExpr _ (Star)      = "*"
    showExpr _ (Box)       = "#"
    showExpr s (Var i)     = s i
    showExpr s (Lam i typ e) = "λ " ++ s i ++ " : " ++ showExpr s typ ++ " . " ++ showExpr s e
    showExpr s (Pi i typ e)  = "∀ " ++ s i ++ " : " ++ showExpr s typ ++ " . " ++ showExpr s e
    showExpr s (App e expr)   = "(" ++ showExpr s e ++ ") (" ++ showExpr s expr ++ ")"

    showCtx :: (a -> String) -> Context a -> String
    showCtx s ctx = concat $ intersperse ", " (map (\(n, typ) -> s n ++ " : " ++ showExpr s typ) ctx)


    -- named variable substitution

    -- Given an expression in a, and a context in a, strip labelling and return an indexed DeBruijn expression and context
    index :: (Eq a, Show a) => Expr a -> Context a -> Result (DeBruijnExpr, DeBruijnCtx)
    index e ctx =
      if errs == []
        then mapR (\e' -> (e', ctx'')) expr
        else throwError $ head errs 
        where ctx0 = map (idxCtx (map fst ctx) []) (map snd ctx)
              (errs, ctx') = partitionEithers ctx0
              (expr, ctx'') = (idxExpr (zip (map fst ctx) ctx') [] e, zip (map negate [1..]) ctx')

    -- Index an expression  given a stack of names for an index both globally and locally
    idxCtx :: (Eq a, Show a) => [a] -> [a] -> Expr a -> Result DeBruijnExpr
    idxCtx = idx idxCtx0 length
        where idxCtx0 ctx s v = (v `elemIndex` ctx, v `elemIndex` s)

    -- Index an expression given a mixed-type context and stack of names for an index
    idxExpr :: (Eq a, Show a) => PartialContext a Int -> [a] -> Expr a -> Result DeBruijnExpr
    idxExpr = idx idxExpr0 length
        where idxExpr0 ctx s v = (((==) v . fst) `findIndex` ctx, v `elemIndex` s)

    idx :: (Show a) => ([b] -> [a] -> a -> (Maybe Int, Maybe Int)) -> ([a] -> Int) -> [b] -> [a] -> Expr a -> Result DeBruijnExpr
    idx φ θ ctx = idx0
        where idx0 _ Star = Right $ Star
              idx0 _ Box  = Right $ Box
              idx0 s (Var v) = case (φ ctx s v) of
                (_,        Just idx) -> Right $ Var $ idx
                (Just idx, Nothing)  -> Right $ Var $ -1 - idx
                (Nothing, Nothing)   -> throwError $ "not in scope: " ++ show v
              idx0 s (Lam a typ e) = fmapR (\typ' -> mapR (\e' -> Lam (θ s) typ' e') (idx0 (s ++ [a]) e)) (idx0 s typ)
              idx0 s (Pi a typ e)  = fmapR (\typ' -> mapR (\e' -> Pi (θ s) typ' e') (idx0 (s ++ [a]) e)) (idx0 s typ)
              idx0 s (App e expr)   = fmapR (\e' -> mapR (\expr' -> App e' expr') (idx0 s expr)) (idx0 s e)

    -- Given a DeBruijn expression and context, convert it to named using a named (string) context
    named :: DeBruijnExpr -> DeBruijnCtx -> ShowCtx -> ShowExpr
    named e ctx ctx' = quote e ctx ctx' (map show [0..])

    -- Given a DeBruijn expression and context, convert it to named using a context and local stack
    quote :: (Eq a) => DeBruijnExpr -> DeBruijnCtx -> Context a -> [a] -> Expr a
    quote e ctx ctx' s = quote0 e
        where quote0 Star = Star
              quote0 Box  = Box
              quote0 (Var v) = case (((==) v . fst) `find` ctx) of
                  Just (i, _) -> Var (fst $ ctx' !! (-1 - i))
                  Nothing     -> Var (s !! v)
              quote0 (Lam a typ e) = Lam (s !! a) (quote0 typ) (quote0 e)
              quote0 (Pi a typ e)  = Pi (s !! a) (quote0 typ) (quote0 e)
              quote0 (App e expr)   = App (quote0 e) (quote0 expr)


    -- core functions wrapped in namedexprs

    -- normalize a named expression
    nNormalizeOf :: ShowExpr -> ShowCtx -> Result ShowExpr
    nNormalizeOf expr typCtx = case index expr typCtx of
        Right (dbExpr, dbCtx) -> Right eval
            where dbEval = normalize dbExpr
                  eval = named dbEval dbCtx typCtx
        Left err -> Left err

    -- `normalizeIn` is the same as `normalizeOf` but in a context binding free names to expressions.
    nNormalizeIn :: ShowCtx -> ShowExpr -> ShowCtx -> Result ShowExpr
    nNormalizeIn eCtx expr = nNormalizeOf (foldl (\expr (i, e) -> substitute i e expr) expr eCtx)

    -- type a named expression
    nTypeOf :: ShowExpr -> ShowCtx -> Result ShowExpr
    nTypeOf expr typCtx = case index expr typCtx of
        Right (dbExpr, dbCtx) -> case inferType dbCtx dbExpr of
            Right dbType -> Right type'
                where type' = named dbType dbCtx typCtx
            Left err -> Left err
        Left err -> Left err

    -- `inferType` is the same as `typeOf` but in a context binding free names to expressions
    nTypeIn :: ShowCtx -> ShowExpr -> ShowCtx -> Result ShowExpr
    nTypeIn eCtx expr = nTypeOf (foldl (\expr (i, e) -> substitute i e expr) expr eCtx)
