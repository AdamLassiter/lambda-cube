module L3.TestCore (tests) where

import qualified L3.Core.TestDeBruijn as TestDeBruijn
import qualified L3.Core.TestEq as TestEq
import qualified L3.Core.TestEval as TestEval
import qualified L3.Core.TestInfer as TestInfer
import qualified L3.Core.TestNormal as TestNormal
import qualified L3.Core.TestShow as TestShow

tests :: [IO ()]
tests = 
  foldl1
    (++)
    [ TestDeBruijn.tests,
      TestEq.tests,
      TestEval.tests,
      TestInfer.tests,
      TestNormal.tests,
      TestShow.tests
      ]
