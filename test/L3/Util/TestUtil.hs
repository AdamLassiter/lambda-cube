module L3.Util.TestUtil (tests) where

import L3.Util
import Test

tests :: [IO ()]
tests =
  [ testshowIndent,
    testThrowError,
    testRethrowError,
    testUnpack,
    testMapL,
    testMapR,
    testFmapR,
    testFlatten,
    testThrowL
  ]

testshowIndent :: IO ()
testshowIndent = do
  assertEq "TestUtil" (showIndent "msg") "| \"msg\"" "Show single-line indent line"
  assertEq "TestUtil" (unlines $ map showIndent ["line1", "line2"]) "| \"line1\"\n| \"line2\"\n" "Show single-line indent line"

testThrowError :: IO ()
testThrowError = do
  assertEq "TestUtil" (show $ throwError ["msg"]) "\n\t msg" "Show single-line error"
  assertEq "TestUtil" (show $ throwError ["line1", "line2"]) "\n\t line1\n\t line2" "Show multiline error"

testRethrowError :: IO ()
testRethrowError = do
  assertEq "TestUtil" (show $ rethrowError ["outer"] (throwError ["inner"])) "\n\t outer\n\n\t inner" "Show nested outer and inner error"
  assertEq "TestUtil" (show $ rethrowError ["outer"] (throwError ["inner", "\tindented"])) "\n\t outer\n\n\t inner\n\t \tindented" "Show nested outer and inner error with indentation"

testUnpack :: IO ()
testUnpack = do
  assertEq "TestUtil" (unpack [Right 0, Right 1, Right 2]) (Right [0, 1, 2]) "Unpack all rights into right"
  assertEq "TestUtil" (unpack [Right 0, Right 1, Left $ throwError ["last error"]]) (Left $ throwError ["last error"]) "Unpack last (left) into left"
  assertEq "TestUtil" (unpack [Left $ throwError ["first error"], Right 1, Right 2]) (Left $ throwError ["first error"]) "Unpack first (left) into left"
  assertEq "TestUtil" (unpack [Left $ throwError ["first error"], Right 1, Left $ throwError ["last error"]]) (Left $ throwError ["first error"]) "Unpack first (first left) into left"

testMapL :: IO ()
testMapL = do
  assertEq "TestUtil" (mapL (\e -> throwError ["mapped error"]) (Left $ throwError ["initial error"] :: Result Int)) (Left $ throwError ["mapped error"]) "mapL maps left"
  assertEq "TestUtil" (mapL (\e -> throwError ["mapped error"]) (Right 0)) (Right 0) "mapL does not map right"

testMapR :: IO ()
testMapR = do
  assertEq "TestUtil" (mapR (const 1) (Left $ throwError ["initial error"] :: Result Int)) (Left $ throwError ["initial error"]) "mapR does not map left"
  assertEq "TestUtil" (mapR (const 1) (Right 0)) (Right 1) "mapR maps right"

testFmapR :: IO ()
testFmapR = do
  assertEq "TestUtil" (fmapR (\i -> Left $ throwError ["flatmapped error"] :: Result Int) (Right 0)) (Left $ throwError ["flatmapped error"]) "fmapR flat maps right into left"
  assertEq "TestUtil" (fmapR (\i -> Right 1) (Right 0)) (Right 1) "fmapR flat maps right into right"

testFlatten :: IO ()
testFlatten = do
  assertEq "TestUtil" (flatten $ Left $ throwError ["error"] :: Result Int) (Left $ throwError ["error"]) "flatten is id on left"
  assertEq "TestUtil" (flatten $ Right $ Left $ throwError ["error"] :: Result Int) (Left $ throwError ["error"]) "flatten unwraps right left into just left"
  assertEq "TestUtil" (flatten $ Right $ Right 0) (Right 0) "flatten unwraps right right into just right"

testThrowL :: IO ()
testThrowL = do
  assertEq "TestUtil" (throwL $ Right 0) 0 "throwL unwraps right"
