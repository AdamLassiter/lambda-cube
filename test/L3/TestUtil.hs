
module L3.TestUtil (tests) where
    import TestUtil
    import L3.Util


    tests :: [IO ()]
    tests = [ testShowIdent
            , testThrowError
            ]

    testShowIdent :: IO ()
    testShowIdent = do
        assertEq (showIdent "msg") "| \"msg\"" "Show single-line indent line"
        assertEq (unlines $ map showIdent ["line1", "line2"]) "| \"line1\"\n| \"line2\"\n" "Show single-line indent line"

    testThrowError :: IO ()
    testThrowError = do
        assertEq (show $ throwError ["msg"]) "\nmsg" "Show single-line error"
        assertEq (show $ throwError ["line1", "line2"]) "\nline1\nline2" "Show multiline error"
        assertEq (show $ throwError ["line1", "\tline2"]) "\nline1\n\tline2" "Show multiline error with indentation"

    testRethrowError :: IO ()
    testRethrowError = do
        assertEq (show $ rethrowError ["outer"] (throwError ["inner"])) "\nouter\n\tinner" "Show nested outer and inner error"
        assertEq (show $ rethrowError ["outer"] (throwError ["inner", "\tindented"])) "\nouter\n\tinner\n\t\tindented" "Show nested outer and inner error with indentation"

    testUnpack :: IO ()
    testUnpack = do
        assertEq (unpack [Right 0, Right 1, Right 2]) (Right [0, 1, 2]) "Unpack all rights into right"
        assertEq (unpack [Right 0, Right 1, Left $ throwError ["last error"]])  (Left $ throwError ["last error"]) "Unpack last (left) into left"
        assertEq (unpack [Left $ throwError ["first error"], Right 1, Right 2])  (Left $ throwError ["first error"]) "Unpack first (left) into left"
        assertEq (unpack [Left $ throwError ["first error"], Right 1, Left $ throwError ["last error"]])  (Left $ throwError ["first error"]) "Unpack first (first left) into left"

    testMonadicError :: IO ()
    testMonadicError = do
        assertEq (mapL (\e -> throwError ["mapped error"]) (Left $ throwError ["initial error"] :: Result Int)) (Left $ throwError ["mapped error"]) "mapL maps left"
        assertEq (mapL (\e -> throwError ["mapped error"]) (Right 0)) (Right 0) "mapL does not map right"

        assertEq (mapR (const 1) (Left $ throwError ["initial error"] :: Result Int)) (Left $ throwError ["initial error"]) "mapR does not map left"
        assertEq (mapR (const 1) (Right 0)) (Right 1) "mapR maps right"

        assertEq (fmapR (\i -> Left $ throwError ["flatmapped error"] :: Result Int) (Right 0)) (Left $ throwError ["flatmapped error"]) "fmapR flat maps right into left"
        assertEq (fmapR (\i -> Right 1) (Right 0)) (Right 1) "fmapR flat maps right into right"

        assertEq (flatten $ Left $ throwError ["error"] :: Result Int) (Left $ throwError ["error"]) "flatten is id on left"
        assertEq (flatten $ Right $ Left $ throwError ["error"] :: Result Int) (Left $ throwError ["error"]) "flatten unwraps right left into just left"
        assertEq (flatten $ Right $ Right 0) (Right 0) "flatten unwraps right right into just right"

        assertEq (throwL $ Right 0) 0 "throwL unwraps right"
