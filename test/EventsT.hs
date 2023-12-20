module EventsT where

import Data.Set (Set)
import Data.Set qualified as Set
import Events (genRandomEvent)
import Resources
import System.Random (mkStdGen)
import Test.HUnit
  ( Test (TestCase, TestList),
    assertBool,
    assertEqual,
    runTestTT,
  )

-- BEGIN: Modifier tests

-- | A Modifier stores:
-- 1. A ResourceType we want to change
-- 2. A Bool, True if we want to add, False if we want to subtract
-- 3. A Nat, by how much

-- -- | Property: Adding a positive amount of a resource increases the total amount
-- prop_AddingPositiveIncreasesTotal :: Resources s -> Modifier -> Property
-- prop_AddingPositiveIncreasesTotal r (M (rt, b, n)) =
--   b ==>
--     let updatedResources = applyModifier r (M (rt, True, n))
--      in getResourceAmount updatedResources rt == getResourceAmount r rt + n

-- -- | Property: Subtracting a positive amount of a resource decreases the total amount
-- prop_SubtractingPositiveDecreasesTotal :: Resources s -> Modifier -> Property
-- prop_SubtractingPositiveDecreasesTotal r (M (rt, b, n)) =
--   not b ==>
--     let updatedResources = applyModifier r (M (rt, False, n))
--      in getResourceAmount updatedResources rt == fromMaybe 0 (getResourceAmount r rt `minus` n)

-- -- | Property: Applying a modifier with zero amount does not change the resource amount
-- prop_ZeroAmountDoesNotChangeResource :: Resources s -> Modifier -> Property
-- prop_ZeroAmountDoesNotChangeResource r (M (rt, _, n)) =
--   n == 0 ==>
--     let updatedResources = applyModifier r (M (rt, True, n))
--      in getResourceAmount updatedResources rt == getResourceAmount r rt

-- END: Modifier tests

-- BEGIN: Event tests

-- eventHunting :: Event
-- eventHunting = E ("Hunting", (M (Food, True, 10), keepClothes, keepMoney))

testEventDescription :: Test
testEventDescription =
  TestCase $
    assertEqual "Event description" "Hunting" (eventString eventHunting)

testEventOutcome :: Test
testEventOutcome =
  TestCase $
    let huntingResources = (M (Food, True, 10), keepClothes, keepMoney)
     in let hunting = applyEvent zeroResources eventHunting
         in assertEqual "Event Outcome" hunting (applyEvent zeroResources eventHunting)

testEventToString :: Test
testEventToString =
  TestCase $
    assertEqual "Event to string" "Hunting: +10 food" (eventToString eventHunting)

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
  let initialResources = zeroResources
  let modifier = genRandomModifier (mkStdGen 42) Food True 10 10 -- Add 10 units of food
  let M (resourceType, isAdd, amount) = modifier
  let updatedResources =
        if isAdd
          then addResources' initialResources resourceType amount
          else substractResources initialResources resourceType amount
  assertEqual "Food amount should increase by 10" 10 (food updatedResources)

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
-}

-- Generate a random event
-- generateRandomEvent :: String -> Event
-- generateRandomEvent s = E (s, generateRandomModifiers)

-- >>> runTestTT eventTests
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}