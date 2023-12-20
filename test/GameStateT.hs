module GameStateT where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Set qualified as Set
import GameState
import Locations
import Options (Command (..), Profession (..))
import Resources
import Data.List (isInfixOf)
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
  let (result, state') = S.runState (runExceptT (performActionM Shop)) initial
  case result of
    Left errMsg -> assertFailure $ "Error during shop action: " ++ errMsg
    Right _ -> do
      assertEqual "Food should increase by 10" 50 (food $ resources state')
      assertEqual "Money should decrease accordingly" 90 (money $ resources state')

-- Test if the game correctly processes a rest action.
testRestAction :: Test
testRestAction = TestCase $ do
  let initial = initialGameState {health = Ill}
  let (result, state') = S.runState (runExceptT restActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during rest action: " ++ errMsg
    Right _ -> assertEqual "Health should improve after rest" Healthy (health state')

testVisitNewLocation :: Test
testVisitNewLocation = TestCase $ do
  let state = initialGameState {mileage = 10, visitedSet = initialVisitedSet}
  let (result, state') = S.runState (runExceptT visitNewLocation) state

  case result of
    Left errMsg -> assertFailure $ "Error during visiting new location: " ++ errMsg
    Right _ -> do
      assertBool "MissouriRiver should be in visited set" (MissouriRiver `Set.member` visitedSet state')

-- Test if changing the marksmanship status updates correctly without affecting other state elements.
testUpdateMarksmanship :: Test
testUpdateMarksmanship = TestCase $ do
  let initial = initialGameState {marksmanship = Ace}
  let newState = setTrack Farmer -- Assuming Farmer has Ace marksmanship
  case newState of
    Just state' -> do
      assertEqual "Marksmanship should be Ace for Farmer" Ace (marksmanship state')
      -- Ensure other state elements remain unchanged
      assertEqual "Health should remain unchanged" (health initial) (health state')
      assertEqual "Mileage should remain unchanged" (mileage initial) (mileage state')
    Nothing -> assertFailure "Failed to update marksmanship"

-- Test if the wallet function returns the correct amount of money.
testWalletFunction :: Test
testWalletFunction = TestCase $ do
  let state = initialGameState {resources = initialResources {money = 700}}
  assertEqual "Wallet should return correct amount of money" 700 (wallet state)

-- Test if setting a profession updates resources and marksmanship.
testSetProfession :: Test
testSetProfession = TestCase $ do
  let newState = setTrack Banker -- Assuming Banker has specific resources and marksmanship
  case newState of
    Just state' -> do
      -- Assert specific resources and marksmanship for Banker
      let expectedResources = bankerResources
      let expectedMarksmanship = Shaky
      assertEqual "Expected resources for Banker" expectedResources (resources state')
      assertEqual "Expected marksmanship for Banker" expectedMarksmanship (marksmanship state')
    Nothing -> assertFailure "Failed to set profession"

-- Test if the player's location is correctly updated and displayed.
testPrintLocation :: Test
testPrintLocation = TestCase $ do
  let state = initialGameState {mileage = 10} -- Mileage corresponding to a specific location
  let locationString = printLocation state
  assertBool "Location string should contain the correct location name" ("Missouri River" `isInfixOf` locationString)

-- Test if the game state correctly updates the date and mileage at fast pace.
testUpdateGameStateFastPace :: Test
testUpdateGameStateFastPace = TestCase $ do
  let state = initialGameState {date = 1, mileage = 100, pace = Fast, resources = initialResources {food = 10}}
  let (result, state') = S.runState (runExceptT updateGameStateM) state
  case result of
    Left errMsg -> assertFailure $ "Error during state update at Fast pace: " ++ errMsg
    Right _ -> do
      assertEqual "Date should increase by 1" 2 (date state')
      assertEqual "Mileage should increase by 145 for Fast pace" 245 (mileage state')

-- Test if the game ends when the money resource is depleted.
testGameEndNoMoney :: Test
testGameEndNoMoney = TestCase $ do
  let state = initialGameState {resources = initialResources {money = 0}}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end money check: " ++ errMsg
    Right end -> assertBool "Game should end when money is depleted" end

-- Test if the rest action does not change health status when already healthy.
testRestActionHealthy :: Test
testRestActionHealthy = TestCase $ do
  let initial = initialGameState {health = Healthy}
  let (result, state') = S.runState (runExceptT restActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during rest action when healthy: " ++ errMsg
    Right _ -> assertEqual "Health should remain Healthy after rest" Healthy (health state')

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
      testRestAction,
      testVisitNewLocation,
      testUpdateMarksmanship,
      testWalletFunction,
      testSetProfession,
      testPrintLocation,
      testUpdateGameStateFastPace,
      testGameEndNoMoney,
      testRestActionHealthy
    ]

runTest :: IO ()
runTest = runTestTT tests >>= print