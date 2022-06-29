module L3.Log.TestLogging (tests) where

import L3.Log
import Test

tests :: [IO ()]
tests =
  [ testMonadicLogs,
    testUnsafeLogs
  ]

testMonadicLogs :: IO ()
testMonadicLogs = do
  debugM "Log::Logging" "Monadic logger at debug"
  infoM "Log::Logging" "Monadic logger at info"
  warnM "Log::Logging" "Monadic logger at warn"

testUnsafeLogs :: IO ()
testUnsafeLogs = do
  assertEq "Log::TestLogging" (traceU "Log::Logging" "Unsafe logger at trace" 1) 1 "Unsafe logger at trace"
  assertEq "Log::TestLogging" (debugU "Log::Logging" "Unsafe logger at debug" 1) 1 "Unsafe logger at debug"
  assertEq "Log::TestLogging" (infoU "Log::Logging" "Unsafe logger at info" 1) 1 "Unsafe logger at info"
  assertEq "Log::TestLogging" (warnU "Log::Logging" "Unsafe logger at warn" 1) 1 "Unsafe logger at warn"
