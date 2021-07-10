-- Pretty-printing for expressions (ie. with Strings instead of Ints)
module L3.Pretty (module L3.Pretty, module L3.Core, module L3.Util) where
    import L3.Core
    import L3.Util

    import Data.List (intercalate)
    import Data.Char (isDigit)


    newtype Name = Name String deriving (Show, Eq)
    instance Enum Name where
        toEnum int = Name $ show int
        fromEnum (Name name) = read $ digitsSuffix name

    type ShowExpr = Expr Name
    -- Show an expression
    showExpr :: ShowExpr -> String
    showExpr Star      = "*"
    showExpr Box       = "#"
    showExpr (Var (Name i)) = i
    showExpr (Lam (Name i) typ e) = "lambda (" ++ i ++ " : " ++ showExpr typ ++ ") . " ++ showExpr e
    showExpr (Pi (Name i) typ e)  = "forall (" ++ i ++ " : " ++ showExpr typ ++ ") . " ++ showExpr e
    showExpr (App e expr)         = "(" ++ showExpr e ++ ") (" ++ showExpr expr ++ ")"

    type ShowCtx = Context Name
    -- Show a context
    prettyShowCtx :: ShowCtx -> String
    prettyShowCtx ctx = intercalate ", " (map (\(Name n, typ) -> n ++ " : " ++ showExpr typ) ctx)


    join :: String -> Int
    join [] = 0
    join (x:xs) = fromEnum x + 256 * join xs

    split :: Int -> String
    -- split x | x > 256 = (toEnum $ x `rem` 256):(split $ x `quot` 256)
    split x = [toEnum x]

    digitsSuffix :: String -> String
    digitsSuffix = reverse . (++ "0") . takeWhile isDigit . reverse
