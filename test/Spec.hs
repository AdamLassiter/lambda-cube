module Main (main) where

import L3.Logging
import qualified L3.TestCore
import qualified L3.TestLexer
import qualified L3.TestLoader
import qualified L3.TestParser
import qualified L3.TestUtil
import qualified Morte.TestMorte

main :: IO ()
main = withStdoutLogging $ do
  setLogLevel LevelDebug
  sequence_ tests

tests :: [IO ()]
tests =
  foldl1
    (++)
    [ map (setDebugSourceRegex ".*Util.*" >>) L3.TestUtil.tests,
      map (setDebugSourceRegex ".*Core.*" >>) L3.TestCore.tests,
      map (setDebugSourceRegex ".*Lexer.*" >>) L3.TestLexer.tests,
      map (setDebugSourceRegex ".*Loader.*" >>) L3.TestLoader.tests,
      map (setDebugSourceRegex ".*Parser.*" >>) L3.TestParser.tests,
      map (setDebugSourceRegex ".*Morte.*" >>) Morte.TestMorte.tests
    ]
