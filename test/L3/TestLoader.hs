module L3.TestLoader (tests) where

import qualified L3.Loader.TestLoader as TestLoader

tests :: [IO ()]
tests = 
  foldl1
    (++)
    [ TestLoader.tests
    ]
