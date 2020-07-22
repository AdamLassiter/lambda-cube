module ParseIO where
    import Core
    import Parsec
    import Util

    import Data.List
    import Control.Applicative hiding (some, many)

    -- pretty-printing for expressions

    showExpr :: (a -> String) -> Expr a -> String
    showExpr _ (Star)      = "*"
    showExpr _ (Box)       = "#"
    showExpr s (Var i)     = s i
    showExpr s (Lam i τ e) = "λ (" ++ s i ++ " : " ++ showExpr s τ ++ ") . " ++ showExpr s e
    showExpr s (Pi i τ e)  = "π (" ++ s i ++ " : " ++ showExpr s τ ++ ") . " ++ showExpr s e
    showExpr s (App e ρ)   = "(" ++ showExpr s e ++ ") (" ++ showExpr s ρ ++ ")"

    showCtx :: (a -> String) -> Context a -> String
    showCtx s ctx = concat $ intersperse ", " (map (\(n, τ) -> s n ++ " : " ++ showExpr s τ) ctx)


    -- equivalent parsing

    parseExpr :: String -> Result NamedExpr
    parseExpr = runParser expr
        where expr = star
                <|> box
                <|> var
                <|> lam
                <|> pi
                <|> app
              star = do
                  reserved "*"
                  return Star
              box  = do
                  reserved "#"
                  return Box
              var  = do
                  v <- word
                  return $ Var v
              lam  = do
                  reserved "λ"
                  (i, τ) <- parens typ
                  reserved "."
                  e <- expr
                  return $ Lam i τ e
              pi   = do
                  reserved "π"
                  (i, τ) <- parens typ
                  reserved "."
                  e <- expr
                  return $ Pi i τ e
              app  = do
                  e <- parens expr
                  ρ <- parens expr
                  return $ App e ρ
              typ  = do
                  v <- word
                  reserved ":"
                  τ <- expr
                  return $ (v, τ)


    -- named variable substitution

    index :: (Eq a, Show a) => Expr a -> Context a -> (DeBruijnExpr, DeBruijnCtx)
    index e γ = (idxExpr (zip (map fst γ) ctx) [] e, zip (map negate [1..]) ctx)
        where ctx = map (idxCtx (map fst γ) []) (map snd γ)

    idxCtx :: (Eq a, Show a) => [a] -> [a] -> Expr a -> DeBruijnExpr
    idxCtx = idx idxCtx0 length
        where idxCtx0 γ s v = (v `elemIndex` γ, v `elemIndex` s)

    idxExpr :: (Eq a, Show a) => PartialContext a Int -> [a] -> Expr a -> DeBruijnExpr
    idxExpr = idx idxExpr0 length
        where idxExpr0 γ s v = (((==) v . fst) `findIndex` γ, v `elemIndex` s)

    idx :: (Show a) => ([b] -> [a] -> a -> (Maybe Int, Maybe Int)) -> ([a] -> Int) -> [b] -> [a] -> Expr a -> DeBruijnExpr
    idx φ θ γ = idx0
        where idx0 _ Star = Star
              idx0 _ Box  = Box
              idx0 s (Var v) = case (φ γ s v) of
                (_,        Just idx) -> Var $ idx
                (Just idx, Nothing)  -> Var $ -1 - idx
                (Nothing, Nothing)   -> error $ "not in scope: " ++ show v
              idx0 s (Lam a τ e) = Lam (θ s) (idx0 s τ) (idx0 (s ++ [a]) e)
              idx0 s (Pi a τ e)  = Pi (θ s) (idx0 s τ) (idx0 (s ++ [a]) e)
              idx0 s (App e ρ)   = App (idx0 s e) (idx0 s ρ) 

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
