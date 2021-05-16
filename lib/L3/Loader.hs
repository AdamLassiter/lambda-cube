module L3.Loader where
    import Prelude hiding (FilePath)

    import Paths_lambda_cube
    import L3.Core
    import L3.Parser
    import L3.Pretty
    import L3.Util

    import Data.Bifunctor
    import Data.Foldable
    import System.Directory.Extra
    import System.FilePath

    data PreludeFile = PreludeType Name FilePath
                     | PreludeExpr Name FilePath
                     deriving (Show)

    data PreludeCtx = TypeCtx ShowCtx
                    | ExprCtx ShowCtx
                     deriving (Show)

    preludeFile :: FilePath -> [PreludeFile]
    preludeFile path = [PreludeExpr (Name $ takeBaseName path) path]

    preludeDir :: [FilePath] -> [PreludeFile]
    preludeDir (p:paths) = case takeBaseName p of
        "@" -> PreludeType (Name $ takeDirectoryName p) p : tail'
        _   -> map namespaceParent (preludeFile p) ++ tail'
        where tail' = preludeDir paths
              takeDirectoryName d = last $ splitPath $ takeDirectory d
              namespaceParent file = case file of
                  PreludeExpr (Name name) path -> PreludeExpr (Name $ takeDirectoryName p ++ "@" ++ name) path
                  PreludeType _ _ -> undefined
    preludeDir [] = []

    readPrelude :: PreludeFile -> IO PreludeCtx
    readPrelude (PreludeType name path) = do
        data' <- readFile path
        let parsed = throwL $ mapL (\err -> "error parsing " ++ show path ++ ": " ++ err) $ parseExpr data'
        return $ TypeCtx [(name, parsed)]
    readPrelude (PreludeExpr name path) = do
        data' <- readFile path
        let parsed = throwL $ mapL (\err -> "error parsing " ++ show path ++ ": " ++ err) $ parseExpr data'
        return $ ExprCtx [(name, parsed)]

    loadPrelude :: IO (ShowCtx, ShowCtx)
    loadPrelude = do
        filesList <- fmap (filter ((== ".l3") . takeExtension)) $ listFiles =<< getDataDir
        dirsList <- fmap (fmap $ filter ((== ".l3") . takeExtension)) $ mapM listFiles =<< listDirectories =<< getDataDir
        let preludes = concat $ map preludeDir dirsList ++ map preludeFile filesList
        ctxes <- mapM readPrelude preludes
        let (tCtx, eCtx) = foldl acc ([], []) ctxes
        putStrLn "Prelude loaded types:"
        forM_ (map (\(Name name, expr) -> name ++ " :: " ++ showExpr expr ++ "\n") tCtx) putStrLn
        putStrLn "Prelude loaded exprs:"
        forM_
          (map (\(Name name, expr) ->
            name ++ " = " ++ showExpr (normalize expr) ++ "\n" ++
            name ++ " : " ++ showExpr (throwL $ inferType tCtx expr) ++ "\n") eCtx) putStrLn
        return (tCtx, eCtx)
             where acc (tAcc, eAcc) x = case x of
                     (TypeCtx t) -> (tAcc ++ t, eAcc)
                     (ExprCtx e) -> (tAcc, eAcc ++ e)

    wrapPrelude :: IO (ShowCtx, ShowExpr -> ShowExpr)
    wrapPrelude = do
        (tCtx, eCtx) <- loadPrelude
        return (tCtx, foldl (\ f (n, e) x -> Lam n (throwL $ inferType0 e) (f x) `App` e) id eCtx)
