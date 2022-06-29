-- | Utilites for result types and error throwing
module L3.Util.Util where

import Data.Char (isSpace)

newtype Error = Error ([String], Maybe Error)
  deriving (Eq)

instance Show Error where
  show = show' 0
    where
      show' :: Int -> Error -> String
      show' i (Error (errs, cause)) = ("\n" ++) . trimR . unlines $ showErrors i errs : showCause i cause
      showErrors i errs = trimR . unlines $ map (indent i) errs
      indent i = trimR . unlines . map (\l -> unwords $ replicate 1 "\t" ++ [trimR l]) . lines
      showCause i cause = case cause of
        Just c -> [show' (i + 1) c]
        Nothing -> []
      trim = trimL . trimR
      trimR = reverse . trimL . reverse
      trimL = dropWhile isSpace

showIdent :: (Show a) => a -> String
showIdent = ("| " ++) . show

type Result a = Either Error a

throw :: Error -> Result a
throw = Left

throwError :: [String] -> Error
throwError errs = Error (errs, Nothing)

rethrowError :: [String] -> Error -> Error
rethrowError errs cause = Error (errs, Just cause)

unpack :: [Result a] -> Result [a]
unpack (Left err : _) = throw err
unpack (Right r : rs) = case unpack rs of
  Left err -> throw err
  Right rs' -> Right (r : rs')
unpack [] = Right []

mapL :: (Error -> Error) -> Result a -> Result a
mapL f (Left err) = Left $ f err
mapL _ (Right res) = Right res

mapR :: (a -> b) -> Result a -> Result b
mapR _ (Left err) = Left err
mapR f (Right res) = Right $ f res

fmapR :: (a -> Result b) -> Result a -> Result b
fmapR _ (Left err) = Left err
fmapR f (Right res) = f res

flatten :: Result (Result a) -> Result a
flatten (Left err) = Left err
flatten (Right (Left err)) = Left err
flatten (Right (Right res)) = Right res

throwL :: Result a -> a
throwL (Left err) = Prelude.error $ show err
throwL (Right res) = res
