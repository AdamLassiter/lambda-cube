module L3.TestUtil (tests) where

import qualified L3.Util.TestUtil as TestUtil

tests :: [IO ()]
tests = 
  foldl1
    (++)
    [ TestUtil.tests
    ]
