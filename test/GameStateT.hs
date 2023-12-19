module GameStateT where

import Control.Monad.Except (ExceptT, runExceptT)
import GameState
import Resources
import State qualified as S
import Test.HUnit
  ( Test (TestCase, TestList),
    assertBool,
    assertEqual,
    assertFailure,
    runTestTT,
  )

-- Test if the game state correctly updates the date and mileage.
testUpdateGameState :: Test
testUpdateGameState = TestCase $ do
  let state = initialGameState {date = 1, mileage = 100, pace = Slow, resources = initialResources {food = 10}}
  let (result, state') = S.runState (runExceptT updateGameStateM) state
  case result of
    Left errMsg -> assertFailure $ "Error during state update: " ++ errMsg
    Right _ -> do
      assertEqual "Date should increase by 1" 2 (date state')
      assertEqual "Mileage should increase by 10 for Slow pace" 195 (mileage state')

-- Test if the health status updates correctly based on the food resource.
testUpdateHealth :: Test
testUpdateHealth = TestCase $ do
  let state = initialGameState {health = Healthy, resources = initialResources {food = 5, clothes = 0, money = 800}}
  let (result, state') = S.runState (runExceptT updateHealthM) state
  case result of
    Left errMsg -> assertFailure $ "Error during health update: " ++ errMsg
    Right _ -> assertEqual "Health should change to Ill if food is low" Ill (health state')

-- Test if the game correctly identifies when to end based on the mileage.
testIsGameEnd :: Test
testIsGameEnd = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end check: " ++ errMsg
    Right end -> assertBool "Game should end when target mileage reached" end

-- Test if the travel action correctly updates the mileage.
testTravelAction :: Test
testTravelAction = TestCase $ do
  let initial = initialGameState {mileage = 0, pace = Slow, resources = initialResources {food = 10}}
  let (result, state') = S.runState (runExceptT travelActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during travel action: " ++ errMsg
    Right _ -> assertEqual "Slow travel should increase mileage by 10" 95 (mileage state')

-- -- Test if the hunt action correctly increases the food resource.
-- testHuntAction :: Test
-- testHuntAction = TestCase $ do
--   let initial = initialGameState {resources = initialResources {food = 0}}
--   let (result, state') = S.runState (runExceptT shopActionM) initial
--   case result of
--     Left errMsg -> assertFailure $ "Error during hunt action: " ++ errMsg
--     Right _ -> assertEqual "Hunting should increase food by 50" 50 (food (resources state'))

-- Test if the game ends when the mileage reaches the target.
testGameEndMileage :: Test
testGameEndMileage = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end mileage check: " ++ errMsg
    Right end -> assertBool "Game should end when target mileage reached" end

-- Test if the game ends when the health status is critical.
testGameEndHealth :: Test
testGameEndHealth = TestCase $ do
  let state = initialGameState {health = Critical}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end health check: " ++ errMsg
    Right end -> assertBool "Game should end when health is Critical" end

-- Test if resources update correctly, particularly the food resource.
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

-- Test if changing the pace updates the pace correctly without affecting other state elements.
testPaceAction :: Test
testPaceAction = TestCase $ do
  let initial = initialGameState {pace = Slow}
  let (result, state') = S.runState (runExceptT paceActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during pace action: " ++ errMsg
    Right _ -> do
      assertEqual "Pace should toggle from Slow to Fast" Fast (pace state')
      -- Ensure other state elements remain unchanged
      assertEqual "Health should remain unchanged" (health initial) (health state')
      assertEqual "Mileage should remain unchanged" (mileage initial) (mileage state')

-- Test if adding food does not change other resources.
testAddFoodOnly :: Test
testAddFoodOnly = TestCase $ do
  let initial = initialGameState
  let updatedResources = addResources' (resources initial) Food 10
  let state' = initial {resources = updatedResources}
  -- Assert that only food resource is increased
  assertEqual "Adding food should increase food resource" 10 (food $ resources state')
  -- Ensure other resources remain unchanged
  assertEqual "Clothes should remain unchanged" (clothes $ resources initial) (clothes $ resources state')
  assertEqual "Money should remain unchanged" (money $ resources initial) (money $ resources state')

-- Test if the game correctly processes a shop action.
testShopAction :: Test
testShopAction = TestCase $ do
  let initial = initialGameState {resources = initialResources {money = 100}}
  let (result, state') = S.runState (runExceptT (shopActionM' Food True 10)) initial
  case result of
    Left errMsg -> assertFailure $ "Error during shop action: " ++ errMsg
    Right _ -> do
      assertEqual "Food should increase by 10" 10 (food $ resources state')
      assertEqual "Money should decrease accordingly" 90 (money $ resources state')

-- Test if the game correctly processes a rest action.
testRestAction :: Test
testRestAction = TestCase $ do
  let initial = initialGameState {health = Ill}
  let (result, state') = S.runState (runExceptT restActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during rest action: " ++ errMsg
    Right _ -> assertEqual "Health should improve after rest" Healthy (health state')

tests :: Test
tests =
  TestList
    [ testUpdateGameState,
      testUpdateHealth,
      testIsGameEnd,
      testTravelAction,
      testGameEndMileage,
      testGameEndHealth,
      testUpdateResources,
      testPaceAction,
      testAddFoodOnly,
      testShopAction,
      testRestAction
    ]

runTest :: IO ()
runTest = runTestTT tests >>= print