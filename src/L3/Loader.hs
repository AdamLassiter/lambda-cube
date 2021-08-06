{-# LANGUAGE TemplateHaskell #-} -- embedDir for prelude

-- |Load and parse '.l3' files
module L3.Loader (module L3.Loader, module L3.Parser) where
    import Prelude hiding (FilePath, error)

    import L3.Logging
    import L3.Parser

    import Data.FileEmbed (embedDir, embedDirListing, getDir)
    import Data.Bifunctor (first, second, bimap)
    import Data.ByteString (ByteString)
    import Data.List (partition)
    import qualified Data.Text (unpack)
    import Data.Text.Encoding (decodeUtf8)
    import System.FilePath

    debugLoader = debugU "Loader"
    debugLoaderM = debugM "Loader"


    -- |Get the last directory name from a path
    takeDirectoryName :: FilePath -> String
    takeDirectoryName p = debugLoader ("takeDirectoryName " ++ show p) (takeDirectoryName' p)
    takeDirectoryName' = last . splitPath . takeDirectory

    -- |Format a file into either non-namespaced `file` or namespaced `dir`@`file`
    takeNamespacedFileName :: FilePath -> String
    takeNamespacedFileName f = debugLoader ("takeNamespacedFileName " ++ show f) (takeNamespacedFileName' f)
    takeNamespacedFileName' f = case (splitFileName . dropExtension) f of
        ("./", file) -> file
        (ns, file)   -> init ns ++ "@" ++ file

    embeddedPreludeIO :: IO [(FilePath, ByteString)]
    embeddedPreludeIO = debugLoaderM "embeddedPreludeIO" >> embeddedPreludeIO'
    embeddedPreludeIO' = filter ((== ".l3") . takeExtension . fst) <$> getDir "prelude"

    -- |Embed and retrieve all '.l3' files in the prelude directory
    embeddedPrelude :: [(FilePath, ByteString)]
    embeddedPrelude = debugLoader "embeddedPrelude" embeddedPrelude'
    embeddedPrelude' = filter ((== ".l3") . takeExtension . fst) $(embedDir "prelude")

    -- |Lex and parse the prelude into a type-context and expression-context
    loadPrelude :: [(FilePath, ByteString)] -> (ShowCtx, ShowCtx)
    loadPrelude embedded = debugLoader ("loadPrelude " ++ show embedded) (loadPrelude' embedded)
    loadPrelude' embedded = (tCtx, eCtx)
        where preludeExprs = map (second (throwL . parseExpr . throwL . lexSrc . Data.Text.unpack . decodeUtf8)) embedded
              types = filter ((== "@") . takeBaseName . fst) preludeExprs
              tCtx = map (bimap (Name . takeDirectoryName) (throwL . inferType0)) types
              eCtx = map (first (Name . takeNamespacedFileName)) preludeExprs

    -- |Fold the prelude context through lambda application into a type-context and expression-context-mapper.
    -- |That is, `let a = x in b` <=> `(λ a -> b) x`
    wrapPrelude :: [(FilePath, ByteString)] -> (ShowCtx, ShowExpr -> ShowExpr)
    wrapPrelude embedded = debugLoader ("wrapPrelude " ++ show embedded) (wrapPrelude' embedded)
    wrapPrelude' embedded = (tCtx, foldl (\ f (n, e) x -> Lam n (throwL $ inferType0 e) (f x) `App` e) id eCtx)
        where (tCtx, eCtx) = loadPrelude embedded
