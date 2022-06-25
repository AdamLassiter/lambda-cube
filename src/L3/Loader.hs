-- embedDir for prelude
{-# LANGUAGE TemplateHaskell #-}

-- | Load and parse '.l3' files
module L3.Loader (module L3.Loader, module L3.Parser) where

import Data.Bifunctor (bimap, first, second)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedDirListing, getDir)
import Data.List (partition)
import qualified Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import L3.Logging
import L3.Parser
import System.FilePath
import Prelude hiding (FilePath, error)

debugLoader = debugU "Loader"

debugLoaderM = debugM "Loader"

-- | Get the last directory name from a path
takeDirectoryName :: FilePath -> String
takeDirectoryName p = debugLoader ("takeDirectoryName " ++ show p) (takeDirectoryName' p)

takeDirectoryName' = last . splitPath . takeDirectory

-- | Format a file into either non-namespaced `file` or namespaced `dir`@`file`
takeNamespacedFileName :: FilePath -> String
takeNamespacedFileName f = debugLoader ("takeNamespacedFileName " ++ show f) (takeNamespacedFileName' f)

takeNamespacedFileName' f = case (splitFileName . dropExtension) f of
  ("./", file) -> file
  (ns, "@") -> init ns
  (ns, file) -> init ns ++ "@" ++ file

embeddedPreludeIO :: IO [(FilePath, ByteString)]
embeddedPreludeIO = debugLoaderM "embeddedPreludeIO" >> embeddedPreludeIO'

embeddedPreludeIO' = filter ((== ".l3") . takeExtension . fst) <$> getDir "prelude"

-- | Embed and retrieve all '.l3' files in the prelude directory
embeddedPrelude :: [(FilePath, ByteString)]
embeddedPrelude = debugLoader "embeddedPrelude" embeddedPrelude'

embeddedPrelude' = filter ((== ".l3") . takeExtension . fst) $(embedDir "prelude")

-- | Lex and parse the prelude into a type-context and expression-context
loadPrelude :: [(FilePath, ByteString)] -> (ShowCtx, ShowCtx)
loadPrelude embedded = debugLoader ("loadPrelude " ++ show embedded) (loadPrelude' embedded)

loadPrelude' embedded = (Ctx tCtx, Ctx eCtx)
  where
    preludeExprs = map (second (throwL . parseExpr . throwL . lexSrc . Data.Text.unpack . decodeUtf8)) embedded
    types = filter ((== "@") . takeBaseName . fst) preludeExprs
    tCtx = map (bimap (Name . takeDirectoryName) (throwL . inferType0)) types
    eCtx = map (first (Name . takeNamespacedFileName)) preludeExprs

-- | Fold the prelude context through lambda application into a type-context and expression-context-mapper.
--  That is, `let a = x in b` <=> `(λ a -> b) x`
wrapPrelude :: [(FilePath, ByteString)] -> (ShowCtx, ShowExpr -> ShowExpr)
wrapPrelude embedded = debugLoader ("wrapPrelude " ++ show embedded) (wrapPrelude' embedded)

wrapPrelude' embedded = (Ctx tCtx, foldl (\f (n, e) x -> Lam n (throwL $ inferType0 e) (f x) `App` e) id eCtx)
  where
    (Ctx tCtx, Ctx eCtx) = loadPrelude embedded
