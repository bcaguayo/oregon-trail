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

-- BEGIN: Modifier tests

-- | A Modifier stores:
-- 1. A ResourceType we want to change
-- 2. A Bool, True if we want to add, False if we want to subtract
-- 3. A Nat, by how much

-- -- -- | Property: Adding a positive amount of a resource increases the total amount
-- -- prop_AddingPositiveIncreasesTotal :: Resources s -> Modifier -> Property
-- -- prop_AddingPositiveIncreasesTotal r (M (rt, b, n)) =
----    b ==>
--     let updatedResources = applyModifier r (M (rt, True, n))
----       in getResourceAmount updatedResources rt == getResourceAmount r rt + n

-- -- -- | Property: Subtracting a positive amount of a resource decreases the total amount
-- -- prop_SubtractingPositiveDecreasesTotal :: Resources s -> Modifier -> Property
-- -- prop_SubtractingPositiveDecreasesTotal r (M (rt, b, n)) =
----    not b ==>
--     let updatedResources = applyModifier r (M (rt, False, n))
----       in getResourceAmount updatedResources rt == fromMaybe 0 (getResourceAmount r rt `minus` n)

-- -- -- | Property: Applying a modifier with zero amount does not change the resource amount
-- -- prop_ZeroAmountDoesNotChangeResource :: Resources s -> Modifier -> Property
-- -- prop_ZeroAmountDoesNotChangeResource r (M (rt, _, n)) =
----    n == 0 ==>
--     let updatedResources = applyModifier r (M (rt, True, n))
----       in getResourceAmount updatedResources rt == getResourceAmount r rt

-- END: Modifier tests

-- BEGIN: Event tests

-- eventTests :: Test
-- eventTests = TestList
--     [ testEventDescription
--     , testEventOutcome
--     , testEventToString
--     ]

-- eventHunting :: Event
-- eventHunting = E ("Hunting", (M (Food, True, 10), keepClothes, keepMoney))

-- testEventDescription :: Test
-- testEventDescription = TestCase $
--     assertEqual "Event description" "Hunting" (show eventHunting)

-- testEventOutcome :: Test
-- testEventOutcome = TestCase $
--     let huntingResources = (M (Food, True, 10), keepClothes, keepMoney) in
--     let hunting = applyEvent 0 eventHunting zeroResources in
--     assertEqual "Event Outcome" hunting (applyEvent 0 eventHunting zeroResources)

-- testEventToString :: Test
-- testEventToString = TestCase $
--     assertEqual "Event to string" "Hunting: +10 food" (show eventHunting)

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

tests :: Test
tests =
  TestList
    [ testRandomEventGeneration,
      testRandomModifierGeneration,
      testEventOutcomes,
      testApplyModifier
    ]

-- END: Event tests
runTest :: IO ()
runTest = runTestTT tests >>= print

-- genRandomModifier :: Nat -> Modifier
-- genRandomModifier n = do
--     let gen = mkStdGen 42
--         (value, gen') = randomR (0, n) gen
--         (positive, gen'') = random gen'
--         resourceType = case randomR (0, 2) gen'' of
--             0 -> Food
--             1 -> Clothes
--             _ -> Money
--     return (M (resourceType, positive, value))

-- genBoundedModifier :: ResourceType -> Nat -> Modifier
-- genBoundedModifier res n = do
--     (gen, _) <- newStdGen
--     let (value, gen') = randomR (0, n) gen
--         (positive, gen'') = random gen'
--     return (M (res, positive, value))

-- generateRandomModifier :: RandomGen g => g -> Int -> (Modifier, g)
-- generateRandomModifier gen n =
--     let (value, gen') = randomR (0, n) gen
--         (isPositive, gen'') = random gen'
--         resourceType = case randomR (0, 2) gen'' of
--             0 -> Food
--             1 -> Clothes
--             _ -> Money
--     in (M (resourceType, isPositive, fromIntegral value), gen'')

-- generateBoundedModifier :: RandomGen g => g -> ResourceType -> Nat -> (Modifier, g)
-- generateBoundedModifier gen res n =
--     let (value, gen') = randomR (0, n) gen
--         (isPositive, gen'') = random gen'
--     in (M (res, isPositive, fromIntegral value), gen'')

-- generateEvent :: IO()
-- generateEvent =

-- generateRandomModifiers :: IO ResourceSet
-- generateRandomModifiers = do
--     gen <- newStdGen
--     let (m1, gen1) = generateRandomModifier gen
--         (m2, gen2) = generateRandomModifier gen1
--         (m3, _) = generateRandomModifier gen2
--     return (m1, m2, m3)

{-
generateRandomMoneyModifier :: RandomGen g => g -> Int -> (Modifier, g)
generateRandomMoneyModifier gen n =
    let (value, gen') = randomR (0, n) gen
        (isPositive, gen'') = random gen'
    in (M (Money, isPositive, fromIntegral value), gen'')

genRandomModifier :: RandomGen g => g -> ResourceType -> Int -> Modifier
genRandomModifier gen res n =
    let (value, gen') = randomR (0, n) gen
        (isPositive, gen'') = random gen'
    in M (res, isPositive, fromIntegral value)

{-
If succesful shopping return new game state
If not succesful shopping return error message "Not enought money"
-}

-- shopActionM' :: ResourceType -> Nat -> GameStateM ()
-- shopActionM' resourceType amount = undefined
  -- do
  -- gs <- lift S.get -- Get the current game state.
  -- let currentResources = resources gs

  -- -- Calculate the cost of the transaction.
  -- let cost = amount * 5 -- Assuming a unit cost of 5 for all resources.

  -- -- Update resources based on whether the player is buying or selling.
  -- updatedResources <- do
  --       -- If buying, add the purchased resource.
  --   let resourcesAfterAddition = addResources' currentResources resourceType amount

  --   -- Try to subtract the equivalent amount of money.
  --   eitherResult <- runExceptT $ substractResources resourcesAfterAddition Money cost
  --   case eitherResult of
  --     Left errMsg -> throwError errMsg -- Throw error if funds are insufficient.
  --     Right newResources -> return newResources

  -- -- Update the game state with the new resources.
  -- lift $ S.put $ gs {resources = updatedResources}

-- shopping :: GameState -> ResourceType -> Nat -> GameState
-- shopping gs res amount = do
--   let currentResources = resources gs
--       totalCost = amount * 10
--       newResources =
--         case addResources currentResources res amount of
--           Left errMsg -> throwError errMsg
--           Right updatedResources -> case runExceptT (substractResources updatedResources Money totalCost) of
--             Left errMsg' -> throwError errMsg'
--             Right updatedResources' -> updatedResources'

  -- do
  -- gs <- lift S.get
  -- let (result, newGs) = runExceptT (shopActionM' res amount)
  -- case result of
  --   Left errMsg -> throwError errMsg
  --   Right _ -> lift (S.put newGs)

-- let shoppingResult =  S.runState (runExceptT (shopActionM' Food natFood)) gs
-- case shoppingResult of
--   (Left errMsg, _) -> output errMsg >> shopFood gs
--   (Right _, updatedGameState) -> output "You have bought some food \n" >> shopClothes updatedGameState
-}

-- Generate a random event
-- generateRandomEvent :: String -> Event
-- generateRandomEvent s = E (s, generateRandomModifiers)

-- >>> runTestTT eventTests
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}