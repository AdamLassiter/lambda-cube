module Constructions where
    --Roughly based on https://github.com/Gabriel439/Haskell-Morte-Library/blob/master/src/Morte/Core.hs by Gabriel Gonzalez et al.

    data Expr = Star | Box | Var Int | Lam Int Expr Expr | Pi Int Expr Expr | App Expr Expr deriving (Show, Eq)

    subst :: Int -> Expr -> Expr -> Expr
    subst v e (Var v')       | v == v' = e
    subst v e (Lam v' ta b ) | v == v' = Lam v' (subst v e ta)            b
    subst v e (Lam v' ta b )           = Lam v' (subst v e ta) (subst v e b )
    subst v e (Pi  v' ta tb) | v == v' = Pi  v' (subst v e ta)            tb
    subst v e (Pi  v' ta tb)           = Pi  v' (subst v e ta) (subst v e tb)
    subst v e (App f a     )           = App    (subst v e f ) (subst v e a )
    subst v e e' = e'

    free v e = e /= subst v (Var $ v + 1) e

    normalize :: Expr -> Expr
    normalize (Lam v ta b) = case normalize b of
        App vb (Var v') | v == v' && not (free v vb) -> vb --Eta reduce
        b' -> Lam v (normalize ta) b'
    normalize (Pi v ta tb) = Pi v (normalize ta) (normalize tb)
    normalize (App f a) = case normalize f of
        Lam v _ b -> subst v (normalize a) b
        f' -> App f' (normalize a)
    normalize c = c

    equiv :: Expr -> Expr -> Bool
    e `equiv` e' = igo (normalize e) (normalize e') (-1) where
        igo (Lam v ta b) (Lam v' ta' b') n = igo ta ta' n && igo (subst v (Var n)  b) (subst v' (Var n)  b') (pred n)
        igo (Pi v ta tb) (Pi v' ta' tb') n = igo ta ta' n && igo (subst v (Var n) tb) (subst v' (Var n) tb') (pred n)
        igo (App f a) (App f' a') n = igo f f' n && igo a a' n
        igo c c' n = c == c'
        

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

    typeOf :: Expr -> Maybe Expr
    typeOf = typeIn []
    wellTyped e = typeOf e /= Nothing