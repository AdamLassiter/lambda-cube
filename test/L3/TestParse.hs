module L3.TestParse (tests) where

import qualified L3.Parse.TestLexer as TestLexer
import qualified L3.Parse.TestParser as TestParser

tests :: [IO ()]
tests = 
  foldl1
    (++)
    [ TestLexer.tests,
      TestParser.tests
    ]
