module Test.Framework(
    UnitTest(..),
    TestResult(..),
    assertEqual,
    assertEqualMargin,
    printResults,
    runTests
) where

import System.IO

data UnitTest = Test String (String -> TestResult)

data TestResult = Passed | Failed String deriving (Show)

assertEqual str a b = if a == b then
    Passed
    else
        Failed ("Failed assertEqual: " ++ str ++ "\n" ++ "On values: " ++ (show a) ++ " " ++ (show b))

assertEqualMargin str a b c = if a-b < c then Passed else Failed (str ++ " " ++ (show a) ++ " " ++ (show b))

printResults results = map printResult results

printResult result = case result of
    Passed -> putStrLn "Passed test case"
    (Failed s) -> putStrLn s

runTests tests = map runTest tests

runTest (Test a b) = b a