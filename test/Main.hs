module Main where

import Test.HUnit
import Test.QuickCheck

import GameStateT

main :: IO()
main = do
    putStrLn "main testing ..."
    runTestTT GameStateT.tests >>= print


-- Test the main function
-- testMain :: Test
-- testMain = TestCase $ do
--     -- Test the output of the main function
--     output <- captureOutput main
--     assertEqual "Unexpected output" "Hello, World!" output

-- Test suite
-- main :: IO ()
-- main = do
--     -- Run the test cases
--     runTestTT $ TestList [testMain]
--     putStrLn "Test suite completed"