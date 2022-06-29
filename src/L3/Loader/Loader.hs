-- embedDir for prelude
{-# LANGUAGE TemplateHaskell #-}

-- | Load and parse '.l3' files
module L3.Loader.Loader
  ( wrapPrelude,
    embeddedPrelude,
    embeddedPreludeIO,
    takeDirectoryName,
    takeNamespacedFileName,
    loadPrelude,
    tauSubst,
    tauNorm,
  )
where

import Data.Bifunctor (bimap, first, second)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedDirListing, getDir)
import Data.List (partition)
import qualified Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import L3.Core
import L3.Log
import L3.Parse
import L3.Util
import System.FilePath
import Prelude hiding (FilePath, error)

debug = debugU "Loader"

debugIO = debugM "Loader"

-- | Get the last directory name from a path
takeDirectoryName :: FilePath -> String
takeDirectoryName p = debug ("takeDirectoryName " ++ show p) (takeDirectoryName' p)

takeDirectoryName' = last . splitPath . takeDirectory

-- | Format a file into either non-namespaced `file` or namespaced `dir`@`file`
takeNamespacedFileName :: FilePath -> String
takeNamespacedFileName f = debug ("takeNamespacedFileName " ++ show f) (takeNamespacedFileName' f)

takeNamespacedFileName' f = case (splitFileName . dropExtension) f of
  ("./", file) -> file
  (ns, "@") -> init ns
  (ns, file) -> init ns ++ "@" ++ file

embeddedPreludeIO :: IO [(FilePath, ByteString)]
embeddedPreludeIO = debugIO "embeddedPreludeIO" >> embeddedPreludeIO'

embeddedPreludeIO' = filter ((== ".l3") . takeExtension . fst) <$> getDir "prelude"

-- | Embed and retrieve all '.l3' files in the prelude directory
embeddedPrelude :: [(FilePath, ByteString)]
embeddedPrelude = debug "embeddedPrelude" embeddedPrelude'

embeddedPrelude' = filter ((== ".l3") . takeExtension . fst) $(embedDir "prelude")

-- | Lex and parse the prelude into a type-context and expression-context
loadPrelude :: [(FilePath, ByteString)] -> (ShowCtx, ShowCtx)
loadPrelude embedded = debug ("loadPrelude " ++ show embedded) (loadPrelude' embedded)

loadPrelude' embedded = (Ctx τ, Ctx ε)
  where
    preludeExprs = map (second (throwL . parseExpr . throwL . lexSrc . Data.Text.unpack . decodeUtf8)) embedded
    types = filter ((== "@") . takeBaseName . fst) preludeExprs
    τ = map (bimap (Name . takeDirectoryName) (throwL . inferType0)) types
    ε = map (first (Name . takeNamespacedFileName)) preludeExprs

-- | Substitute all occurrences of a variable v with an expression e, but only where
-- v appears in a type.
tauSubst :: (Eq a, Enum a, Show a) => a -> Expr a -> Expr a -> Expr a
tauSubst v e e' = debug ("tauSubst " ++ show v ++ ", " ++ show e ++ ", " ++ show e') (tauSubst' v e e')

tauSubst' v e (Lam v' ta b) | v == v' = Lam v' (substitute v e ta) b
tauSubst' v e (Lam v' ta b) = Lam v' (substitute v e ta) (tauSubst v e b)
tauSubst' v e (Pi v' ta tb) | v == v' = Pi v' (substitute v e ta) tb
tauSubst' v e (Pi v' ta tb) = Pi v' (substitute v e ta) (tauSubst v e tb)
tauSubst' v e (App f a) = App (tauSubst v e f) (tauSubst v e a)
tauSubst' _ _ e' = e'

-- | Partially evaluate the types of an expression through lambda-application substitutions.
-- This should remain correct, but allows for binding types and evaluating by-reference.
tauNorm :: (Eq a, Enum a, Show a) => Expr a -> Expr a
tauNorm e = debug ("tauNorm " ++ show e) (tauNorm' e)

tauNorm' (App f a) = case f of
  Lam v ta b -> App (Lam v ta (tauNorm $ tauSubst v (tauNorm a) b)) (tauNorm a)
  otherwise -> App (tauNorm f) (tauNorm a)
tauNorm' (Lam v ta b) = Lam v (tauNorm ta) (tauNorm b)
tauNorm' (Pi v ta b) = Pi v (tauNorm ta) (tauNorm b)
tauNorm' e = e

-- | Fold the prelude context through lambda application into a type-context and expression-context-mapper.
--  That is, `let a = x in b` <=> `(λ a -> b) x`
wrapPrelude :: [(FilePath, ByteString)] -> (ShowCtx, ShowExpr -> ShowExpr)
wrapPrelude embedded = debug ("wrapPrelude " ++ show embedded) (wrapPrelude' embedded)

wrapPrelude' embedded = (Ctx τ, tauNorm . (foldl (\f (n, e) x -> Lam n (throwL $ inferType0 e) (f x) `App` e) id ε))
  where
    (Ctx τ, Ctx ε) = loadPrelude embedded
