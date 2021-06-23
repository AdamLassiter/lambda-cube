{-# OPTIONS_GHC -Wno-all #-}

-- Reverse-parse of Pretty shows
module L3.Lexer (module L3.Lexer, module L3.Parser) where
    import L3.Pretty
    import L3.Parsec
    import L3.Parser
    import L3.Util

    -- parse a string and desugar into core expressions
    --lexExpr :: String -> Result String
    --lexExpr = runParser appE
--
    --appE :: Parser ShowExpr
    --appE = do
    --    expr <- sugarE
    --    exprs <- many sugarE
    --    return $ foldl App expr exprs
