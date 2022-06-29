
module L3.Log.TestColors (tests) where

import qualified Data.Set as Set
import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testColors
  ]

testColors :: IO ()
testColors = do
  let allColors = [reset, black, red, green, yellow, blue, magenta, cyan, white, brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite]
  assertEq "Log::TestColors" (length $ Set.fromList allColors) (length allColors) "There's some colors, all of them unique"
