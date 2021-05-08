module L3.Loader where
    import Paths_lambda_cube
    import L3.Parser
    import L3.Pretty

    import System.Directory.Extra

    loadPrelude :: IO ShowCtx
    loadPrelude = do
        filesList <- listFilesRecursive =<< getDataDir
        print filesList
        return []
