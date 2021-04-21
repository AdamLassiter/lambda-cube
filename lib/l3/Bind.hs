module L3.Bind where
    import L3.Core

    data Label = Free String | Bound DeBruijnExpr

    type BindingExpr = Expr Label
    type BindingContext = PartialContext String BindingExpr

    -- reindex an expression from n upwards
    reindex :: Int -> DeBruijnExpr -> DeBruijnExpr
    reindex i = reindex0 . fmap (+ i)

    reindex1 :: DeBruijnExpr -> DeBruijnExpr
    reindex1 = reindex 1

    -- reindex an expression from 0 upwards
    reindex0 :: DeBruijnExpr -> DeBruijnExpr
    reindex0 expr = fmap (\x -> x - (indexOf expr)) expr

    -- find the least n in an expression
    indexOf :: DeBruijnExpr -> Int
    indexOf = foldr1 min

    evaluate :: BindingExpr -> BindingContext -> (DeBruijnExpr, DeBruijnCtx)
    evaluate = undefined
