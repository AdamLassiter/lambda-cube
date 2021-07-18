{-# LANGUAGE TemplateHaskell #-} -- embedDir for prelude

-- | Load and parse '.l3' files
module L3.Loader (module L3.Loader, module L3.Parser) where
    import Prelude hiding (FilePath)

    import L3.Parser

    import Data.FileEmbed (embedDir, embedDirListing)
    import Data.Bifunctor (first, second, bimap)
    import Data.ByteString (ByteString)
    import Data.List (partition)
    import qualified Data.Text (unpack)
    import Data.Text.Encoding (decodeUtf8)
    import System.FilePath


    -- | Get the last directory name from a path
    takeDirectoryName :: FilePath -> String
    takeDirectoryName = last . splitPath . takeDirectory

    -- | Format a file into either non-namespaced `file` or namespaced `dir`@`file`
    takeNamespacedFileName :: FilePath -> String
    takeNamespacedFileName f = case (splitFileName . dropExtension) f of
        ("./", file) -> file
        (ns, file)   -> init ns ++ "@" ++ file

    -- | Embed and retrieve all '.l3' files in the prelude directory
    embeddedPrelude :: [(FilePath, ByteString)]
    embeddedPrelude = filter ((== ".l3") . takeExtension . fst) $(embedDir "prelude")

    -- | Lex and parse the prelude into a type-context and expression-context
    loadPrelude :: (ShowCtx, ShowCtx)
    loadPrelude = (tCtx, eCtx)
        where preludeExprs = map (second (throwL . parseExpr . throwL . lexSrc . Data.Text.unpack . decodeUtf8)) embeddedPrelude
              types = filter ((== "@") . takeBaseName . fst) preludeExprs
              tCtx = map (bimap (Name . takeDirectoryName) (throwL . inferType0)) types
              eCtx = map (first (Name . takeNamespacedFileName)) preludeExprs

    -- | Fold the prelude context through lambda application into a type-context and expression-context-mapper.
    -- | That is, `let a = x in b` <=> `(λ a -> b) x`
    wrapPrelude :: (ShowCtx, ShowExpr -> ShowExpr)
    wrapPrelude = (tCtx, foldl (\ f (n, e) x -> Lam n (throwL $ inferType0 e) (f x) `App` e) id eCtx)
        where (tCtx, eCtx) = loadPrelude
