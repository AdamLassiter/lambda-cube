{-# LANGUAGE TemplateHaskell #-}

-- Load and parse '.l3' files
module L3.Loader where
    import Prelude hiding (FilePath)

    import L3.Parser (parseExpr)
    import L3.Pretty (ShowCtx, ShowExpr, throwL, inferType0, Expr (Lam, App), Name (Name))

    import Data.FileEmbed (embedDir, embedDirListing)
    import Data.Bifunctor (first, second, bimap)
    import Data.ByteString (ByteString)
    import Data.List (partition)
    import Data.Text (unpack)
    import Data.Text.Encoding (decodeUtf8)
    import System.FilePath

    takeDirectoryName :: FilePath -> String
    takeDirectoryName = last . splitPath . takeDirectory

    takeNamespacedFileName :: FilePath -> String
    takeNamespacedFileName f = case (splitFileName . dropExtension) f of
        ("./", file) -> file
        (ns, file)   -> init ns ++ "@" ++ file

    embeddedPrelude :: [(FilePath, ByteString)]
    embeddedPrelude = filter ((== ".l3") . takeExtension . fst) $(embedDir "prelude")

    loadPrelude :: (ShowCtx, ShowCtx)
    loadPrelude = (tCtx, eCtx)
        where preludeExprs = map (second (throwL . parseExpr . unpack . decodeUtf8)) embeddedPrelude
              types = filter ((== "@") . takeBaseName . fst) preludeExprs
              tCtx = map (bimap (Name . takeDirectoryName) (throwL . inferType0)) types
              eCtx = map (first (Name . takeNamespacedFileName)) preludeExprs

    wrapPrelude :: IO (ShowCtx, ShowExpr -> ShowExpr)
    wrapPrelude = do
        let (tCtx, eCtx) = loadPrelude
        return (tCtx, foldl (\ f (n, e) x -> Lam n (throwL $ inferType0 e) (f x) `App` e) id eCtx)
