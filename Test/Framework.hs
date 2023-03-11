{- |
Module: Test
Description: A testing framework for unit testing of haskell code
-}
module Test.Framework(
    UnitTest(..),
    TestResult(..),
    assertEqual,
    assertEqualMargin,
    runAll
) where

import System.IO

{- | UnitTest type

Define unittest using a string and defining a function with the actual test
-}
data UnitTest = Test String (String -> TestResult)

{- | TestResult Type

Should be the result from the function in a UnitTest type
-}
data TestResult = Passed | Failed String deriving (Show)

{- | Assert that two variables are equal, string is ID for the error message
-}
--assertEqual :: (Eq a, Show a) => [Char] -> a -> a -> TestResult
assertEqual str a b = if a == b then
    Passed
    else
        Failed ("Failed assertEqual: " ++ str ++ "\n" ++ "On values:\n" ++ (show a) ++ "\n" ++ (show b))

{- | Assert that two variables are within a margin, string is ID for the error message
-}
assertEqualMargin str a b c =
    if a-b < c then
        Passed
    else
        Failed ("Failed assertEqualMargin: " ++ str ++ "\n" ++ "On values:\n" ++ (show a) ++ "\n" ++ (show b))

{- | To print the results of all the tests in a nice way
-}
--printResults results = map printResult results
printResults [] = return ()
printResults [a] = printResult a
printResults (x:xs) = do
    printResults xs
    printResult x

printResult result = case result of
    Passed -> putStrLn "Passed test case"
    (Failed s) -> putStrLn s

{- | To run all the unittests
-}
runTests tests = map runTest tests

runTest (Test a b) = b a

runAll tests = let results = runTests tests
                   passed = filter (\x -> checkType x) results
                   failed = filter (\x -> not (checkType x)) results in
    if (length failed) == 0 then
        putStrLn "All tests passed"
    else do
        putStrLn ((show (length passed)) ++ " out of " ++ (show (length tests)) ++ " tests passed")
        putStrLn ""
        printResults failed

checkType x = case x of
    Passed -> True
    Failed _ -> False