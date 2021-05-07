{-# LANGUAGE OverloadedStrings #-}

-- Pretty-printing for expressions (ie. with Strings instead of Ints)
module L3.Pretty (module L3.Pretty, module L3.Core) where
    import L3.Core
    import L3.Util

    import Data.List
    import Data.Either
    import Data.Text.Encoding (encodeUtf8, decodeUtf8)
    import Data.ByteString (pack, unpack)
    import Data.Text (pack, unpack)

    join :: String -> Int
    join [x] = fromEnum x
    join (x:xs) = fromEnum x + 256 * join xs

    split :: Int -> String
    -- split x | x > 256 = (toEnum $ x `rem` 256):(split $ x `quot` 256)
    split x = [toEnum x]

    newtype Name = Name String deriving (Show, Eq)
    instance Enum Name where
        toEnum int = Name $ split int
        fromEnum (Name name) = join name

    type ShowExpr = Expr Name
    type ShowCtx = Context Name

    -- Show an expression using a naming function
    showExpr :: (a -> String) -> Expr a -> String
    showExpr _ Star      = "*"
    showExpr _ Box       = "#"
    showExpr s (Var i)     = s i
    showExpr s (Lam i typ e) = "λ " ++ s i ++ " : " ++ showExpr s typ ++ " . " ++ showExpr s e
    showExpr s (Pi i typ e)  = "∀ " ++ s i ++ " : " ++ showExpr s typ ++ " . " ++ showExpr s e
    showExpr s (App e expr)  = "(" ++ showExpr s e ++ ") (" ++ showExpr s expr ++ ")"

    showCtx :: (a -> String) -> Context a -> String
    showCtx s ctx = intercalate ", " (map (\(n, typ) -> s n ++ " : " ++ showExpr s typ) ctx)
