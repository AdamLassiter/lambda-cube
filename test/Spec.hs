module Main (main) where

import Data.Time.Clock
import L3.Log.Logging
import qualified L3.TestCore
import qualified L3.TestLoader
import qualified L3.TestLog
import qualified L3.TestParse
import qualified L3.TestUtil
import qualified Morte.TestMorte

main :: IO ()
main = withStdoutLogging $ do
  setLogLevel LevelDebug 
  start <- getCurrentTime
  sequence_ tests
  stop <- getCurrentTime
  infoM "Spec" $ "Ran " ++ (show $ length tests) ++ " tests in " ++ (show $ nominalDiffTimeToSeconds $ diffUTCTime stop start) ++ " seconds"

tests :: [IO ()]
tests =
  foldl1
    (++)
    [ map (setDebugSourceRegex ".*(Test|Log).*" >>) L3.TestLog.tests,
      map (setDebugSourceRegex ".*(Test|Util).*" >>) L3.TestUtil.tests,
      map (setDebugSourceRegex ".*(Test|Parse).*" >>) L3.TestParse.tests,
      map (setDebugSourceRegex ".*(Test|Core).*" >>) L3.TestCore.tests,
      map (setDebugSourceRegex ".*(Test|Loader).*" >>) L3.TestLoader.tests,
      map (setDebugSourceRegex ".*(Test|Morte).*" >>) Morte.TestMorte.tests
    ]
