module GameStateT where

import qualified State as S
import GameState
import Resources
import Test.HUnit
    ( assertBool, assertEqual, runTestTT, Test(TestList, TestCase) )

testGameEndMileage' :: Test
testGameEndMileage' = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let isEnd = S.evalState isGameEndM state
  assertBool "Game should end when target mileage reached" isEnd

tests :: Test
tests = TestList [testGameEndMileage']
{-
import Test.HUnit
import Test.QuickCheck

-- import Lib

-- main :: IO ()
-- main = do 
--     putStrLn someFunc
--     putStrLn "Test suite not yet implemented"

-}