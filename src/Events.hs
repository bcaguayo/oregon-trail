module Events where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set as Set (Set, fromList, size, toList)
import Data.Type.Nat
import Resources
  ( ResourceType (Clothes, Food, Medicine, Money, Oxen, Wheels),
    Resources,
  )
import System.Random (Random, RandomGen, mkStdGen, random, randomR)

-- | A Modifier stores:
-- 1. A ResourceType we want to change
-- 2. A Bool, True if we want to add, False if we want to substract
-- 3. A Nat, by how much
newtype Modifier = M {modifier :: (ResourceType, Bool, Nat)} deriving (Eq, Ord)

-- | Examples of modifiers:
keepFood :: Modifier
keepFood = M (Food, True, 0)

keepClothes :: Modifier
keepClothes = M (Clothes, True, 0)

keepMoney :: Modifier
keepMoney = M (Money, True, 0)

-- | Generates a random modifier with given parameters.
genRandomModifier :: (RandomGen g) => g -> ResourceType -> Bool -> Nat -> Nat -> Modifier
genRandomModifier gen rt isPos minAmount maxAmount = do
  let value = fst (randomR (fromIntegral minAmount, fromIntegral maxAmount) gen) :: Int
  M (rt, isPos, fromIntegral value)

loseFood :: Modifier
loseFood = genRandomModifier (mkStdGen 42) Food False 5 20

instance Show Modifier where
  show (M (rt, b, n)) = if n == 0 then "" else sign ++ show n ++ " " ++ show rt
    where
      sign = if b then "+" else "-"

-- | An Outcome represents the possible results of an event. It contains:
-- 1. A description string.
-- 2. A set of modifiers to apply if chosen.
newtype Outcome = O {outcome :: (String, Set Modifier)} deriving (Eq)

instance Show Outcome where
  show (O (s, ms)) =
    if null ms || size ms == 0
      then s ++ ": No effect"
      else s ++ ": " ++ intercalate ", " (map show (toList ms))

-- Example outcomes:
riverOutcomeGood :: Outcome
riverOutcomeGood = O ("Look for a Bridge", Set.fromList [keepFood, keepClothes, keepMoney])

riverOutcomeBad :: Outcome
riverOutcomeBad = O ("Cross the River", fromList [f, c, o])
  where
    f = genRandomModifier (mkStdGen 42) Food False 5 20
    c = genRandomModifier (mkStdGen 42) Clothes False 5 20
    o = genRandomModifier (mkStdGen 42) Oxen False 0 1

huntingOutcome :: Outcome
huntingOutcome = O ("Go Hunting", Set.fromList [genRandomModifier (mkStdGen 42) Food True 20 30])

-- | An Event represents a scenario in the game. It contains:
-- 1. A description of the event.
-- 2. A list of possible outcomes.
newtype Event = E {event :: (String, [Outcome])} deriving (Eq)

instance Ord Event where
  compare (E (s1, _)) (E (s2, _)) = compare s1 s2

-- Print each outcome on a new line
instance Show Event where
  show (E (s, os)) = s ++ "\n" ++ intercalate "\n" (map show os)

-- >>> show eventRiver

-- Examples of events:
eventHunting :: Event
eventHunting = E ("Hunting", [huntingOutcome])

eventRiver :: Event
eventRiver = E ("You Found A River", riverOutcomes)
  where
    riverOutcomes = [riverOutcomeGood, riverOutcomeBad]

eventRaiders :: Event
eventRaiders = E ("A group of Raiders corner your wagon", raidersOutcomes)
  where
    modLoseFoodWorse = genRandomModifier (mkStdGen 42) Food False 30 50
    modLoseFoodBad = genRandomModifier (mkStdGen 42) Food False 10 30
    modLoseClothes = genRandomModifier (mkStdGen 42) Clothes False 30 50
    modLoseMoney = genRandomModifier (mkStdGen 42) Money False 10 50
    raiderOutcomeGood = O ("Fight", fromList [modLoseFoodWorse, modLoseMoney])
    raiderOutcomeBad = O ("Run", fromList [modLoseFoodBad, modLoseClothes])
    raidersOutcomes = [raiderOutcomeGood, raiderOutcomeBad]

-- | A set of all possible events.
eventSet :: Set Event
eventSet = Set.fromList [eventHunting, eventRiver, eventRaiders]

-- | Generates a random event from the set of events.
genRandomEvent :: (RandomGen g) => g -> Event
genRandomEvent gen = do
  let (event, gen') = randomR (0, size eventSet) gen
  fromMaybe (E ("", [])) (lookup event (zip [0 ..] (toList eventSet)))