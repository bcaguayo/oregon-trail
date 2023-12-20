module EventsT where

import Control.Monad.Except
  ( ExceptT (..),
    MonadError (throwError),
    MonadTrans (lift),
    foldM,
    runExceptT,
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Events
import GameState (addResources, applyEvent, applyModifier, applyOutcome, initialGameState, substractResources)
import Resources
import State as S
import System.Random (mkStdGen)
import Test.HUnit
  ( Test (TestCase, TestList),
    assertBool,
    assertEqual,
    assertFailure,
    runTestTT,
  )

-- Test to ensure that a random event is correctly generated from the set of events
testRandomEventGeneration :: Test
testRandomEventGeneration = TestCase $ do
  let gen = mkStdGen 42 -- Fixed seed for reproducibility
  let randomEvent = genRandomEvent gen
  assertBool "Random event should be from the event set" (randomEvent `Set.member` eventSet)

-- Test to ensure that a random modifier is correctly generated within specified parameters
testRandomModifierGeneration :: Test
testRandomModifierGeneration = TestCase $ do
  let gen = mkStdGen 42
  let modifier = genRandomModifier gen Food False 5 20
  let M (_, _, amount) = modifier
  assertBool "Modifier amount should be within range" (amount >= 5 && amount <= 20)

-- Test to ensure that events have the correct structure and outcomes
testEventOutcomes :: Test
testEventOutcomes = TestCase $ do
  let (E (_, outcomes)) = eventRiver
  assertEqual "River event should have 2 outcomes" 2 (length outcomes)

-- Test to simulate applying a modifier to resources and verify the expected change
testApplyModifier :: Test
testApplyModifier = TestCase $ do
  -- Initialize resources and generate a modifier
  let initialResources = zeroResources
  let modifier = genRandomModifier (mkStdGen 42) Food True 10 10 -- Increase food units by 10

  -- Unpack the modifier
  let M (resourceType, isAdd, amount) = modifier

  -- Apply the modifier and handle ExceptT
  let updatedResult =
        if isAdd
          then Right (addResources' initialResources resourceType amount)
          else evalState (runExceptT $ substractResources initialResources resourceType amount) initialGameState

  -- Test the result
  case updatedResult of
    Left errMsg -> assertFailure errMsg -- Fail the test if there's an error
    Right updatedResources -> assertEqual "Food amount should increase by 10" 10 (food updatedResources)

-- Test to ensure that a random event is correctly generated from the set of events
testEventDescription :: Test
testEventDescription = TestCase $ do
  let huntingDescription = show $ event eventHunting
  assertEqual "Event description for Hunting" "(\"Hunting\",[Go Hunting: +20 food])" huntingDescription

  let riverDescription = show $ event eventRiver
  assertEqual "Event description for River" "(\"You Found A River\",[Look for a Bridge: , , ,Cross the River: -5 food, -5 clothes, ])" riverDescription

-- Test to ensure that a random event is correctly generated from the set of events
testEventOutcome :: Test
testEventOutcome = TestCase $ do
  let initialResources = zeroResources
  let (E (_, outcomes)) = eventHunting
  let outcomeHunting = head outcomes
  let (O (_, mods)) = outcomeHunting
  let modifier = head (Set.toList mods)
  let updatedResources = evalState (runExceptT $ applyModifier initialResources modifier) initialGameState
  case updatedResources of
    Left errMsg -> assertFailure errMsg
    Right res -> assertBool "Resource should increase after hunting" (food res > 0)

-- Test to ensure that a random event is correctly generated from the set of events
testRiverEventOutcome :: Test
testRiverEventOutcome = TestCase $ do
  let initialResources = zeroResources
  let (E (_, outcomes)) = eventRiver
  let outcomeRiver = head outcomes -- Look for a bridge 
  let (O (_, mods)) = outcomeRiver
  let modifier = head (Set.toList mods)
  let updatedResources = evalState (runExceptT $ applyModifier initialResources modifier) initialGameState
  case updatedResources of
    Left errMsg -> assertFailure errMsg
    Right res -> assertBool "Resource should change after river event" (clothes res > 0 || oxen res > 0)



tests :: Test
tests =
  TestList
    [ testRandomEventGeneration,
      testRandomModifierGeneration,
      testEventOutcomes,
      testApplyModifier,
      testEventDescription,
      testEventOutcome,
      testRiverEventOutcome
    ]

-- END: Event tests
runTest :: IO ()
runTest = runTestTT tests >>= print