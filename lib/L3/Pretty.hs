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
    showExpr :: ShowExpr -> String
    showExpr Star      = "*"
    showExpr Box       = "#"
    showExpr (Var (Name i)) = i
    showExpr (Lam (Name i) typ e) = "\\ " ++ i ++ " : " ++ showExpr typ ++ " . " ++ showExpr e
    showExpr (Pi (Name i) typ e)  = "forall " ++ i ++ " : " ++ showExpr typ ++ " . " ++ showExpr e
    showExpr (App e expr)         = "(" ++ showExpr e ++ ") (" ++ showExpr expr ++ ")"

    showCtx :: ShowCtx -> String
    showCtx ctx = intercalate ", " (map (\(Name n, typ) -> n ++ " : " ++ showExpr typ) ctx)
