module GameStateT where

import qualified State as S
import GameState
import Control.Monad.Except (ExceptT, runExceptT)
import Resources
import Test.HUnit
    ( assertBool, assertEqual, runTestTT, Test(TestList, TestCase), assertFailure )

testUpdateGameState :: Test
testUpdateGameState = TestCase $ do
  let state = initialGameState {date = 1, mileage = 100, pace = Slow}
  let (result, state') = S.runState (runExceptT updateGameStateM) state
  case result of
    Left errMsg -> assertFailure $ "Error during state update: " ++ errMsg
    Right _ -> do
      assertEqual "Date should increase by 1" 2 (date state')
      assertEqual "Mileage should increase by 10 for Slow pace" 110 (mileage state')

testUpdateHealth :: Test
testUpdateHealth = TestCase $ do
  let state = initialGameState {health = Healthy, resources = initialResources {food = 5, clothes = 0, money = 800}}
  let (result, state') = S.runState (runExceptT updateHealthM) state
  case result of
    Left errMsg -> assertFailure $ "Error during health update: " ++ errMsg
    Right _ -> assertEqual "Health should change to Ill if food is low" Ill (health state')

testIsGameEnd :: Test
testIsGameEnd = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end check: " ++ errMsg
    Right end -> assertBool "Game should end when target mileage reached" end

testTravelAction :: Test
testTravelAction = TestCase $ do
  let initial = initialGameState {mileage = 0, pace = Slow}
  let (result, state') = S.runState (runExceptT travelActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during travel action: " ++ errMsg
    Right _ -> assertEqual "Slow travel should increase mileage by 10" 10 (mileage state')

testHuntAction :: Test
testHuntAction = TestCase $ do
  let initial = initialGameState {resources = initialResources {food = 0}}
  let (result, state') = S.runState (runExceptT shopActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during hunt action: " ++ errMsg
    Right _ -> assertEqual "Hunting should increase food by 30" 30 (food (resources state'))

testGameEndMileage :: Test
testGameEndMileage = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end mileage check: " ++ errMsg
    Right end -> assertBool "Game should end when target mileage reached" end

testGameEndHealth :: Test
testGameEndHealth = TestCase $ do
  let state = initialGameState {health = Critical}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end health check: " ++ errMsg
    Right end -> assertBool "Game should end when health is Critical" end

testUpdateResources :: Test
testUpdateResources = TestCase $ do
  -- use addResources to get initial resources
  let initialResourcesState = runExceptT $ addResources initialResources Food 10
  let (initialResourcesResult, _) = S.runState initialResourcesState initialGameState
  case initialResourcesResult of
    Left errMsg -> assertFailure $ "Error during initial resource setup: " ++ errMsg
    Right updatedResources -> do
      let initialState = initialGameState {resources = updatedResources}
      let (result, state') = S.runState (runExceptT updateResourcesM) initialState
      case result of
        Left errMsg -> assertFailure $ "Error during resource update: " ++ errMsg
        Right _ -> assertEqual "Food should decrease by 5 after update" 5 (food $ resources state')

tests :: Test
tests =
  TestList
    [ testUpdateGameState,
      testUpdateHealth,
      testIsGameEnd,
      testTravelAction,
      testHuntAction,
      testGameEndMileage,
      testGameEndHealth,
      testUpdateResources
    ]

runTest :: IO ()
runTest = runTestTT tests >>= print
{-
import Test.HUnit
import Test.QuickCheck

-- import Lib

-- main :: IO ()
-- main = do 
--     putStrLn someFunc
--     putStrLn "Test suite not yet implemented"

-}