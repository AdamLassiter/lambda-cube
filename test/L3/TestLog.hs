module L3.TestLog (tests) where

import qualified L3.Log.TestColors as TestColors
import qualified L3.Log.TestLogging as TestLogging

tests :: [IO ()]
tests = 
  foldl1
    (++)
    [ TestColors.tests,
      TestLogging.tests
    ]
