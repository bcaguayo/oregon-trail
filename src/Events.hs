module Events where

import Resources
import Data.Type.Nat
import Test.HUnit
import Test.QuickCheck
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

-- | A Modifier stores: 
-- 1. A ResourceType we want to change
-- 2. A Bool, True if we want to add, False if we want to substract
-- 3. A Nat, by how much
newtype Modifier = M { modifier :: (ResourceType, Bool, Nat) } deriving (Eq)

-- | for example:
keepFood :: Modifier
keepFood = M (Food, True, 0)

keepClothes :: Modifier
keepClothes = M (Clothes, True, 0)

keepMoney :: Modifier
keepMoney = M (Money, True, 0)

applyModifier :: Resources s -> Modifier -> Resources s
applyModifier r m = case m of
    M (rt, b, n) -> if b then addResources r rt n else substractResources r rt n

instance Show Modifier where
    show (M (rt, b, n)) = if n == 0 then "" else sign ++ show n ++ " " ++ show rt
        where sign = if b then "+" else "-"

-- BEGIN: Modifier tests
-- | A Modifier stores: 
-- 1. A ResourceType we want to change
-- 2. A Bool, True if we want to add, False if we want to subtract
-- 3. A Nat, by how much

-- | Property: Adding a positive amount of a resource increases the total amount
prop_AddingPositiveIncreasesTotal :: Resources s -> Modifier -> Property
prop_AddingPositiveIncreasesTotal r (M (rt, b, n)) =
    b ==> let updatedResources = applyModifier r (M (rt, True, n))
          in getResourceAmount updatedResources rt == getResourceAmount r rt + n

-- | Property: Subtracting a positive amount of a resource decreases the total amount
prop_SubtractingPositiveDecreasesTotal :: Resources s -> Modifier -> Property
prop_SubtractingPositiveDecreasesTotal r (M (rt, b, n)) =
    not b ==> let updatedResources = applyModifier r (M (rt, False, n))
              in getResourceAmount updatedResources rt == fromMaybe 0 (getResourceAmount r rt `minus` n)

-- | Property: Applying a modifier with zero amount does not change the resource amount
prop_ZeroAmountDoesNotChangeResource :: Resources s -> Modifier -> Property
prop_ZeroAmountDoesNotChangeResource r (M (rt, _, n)) =
    n == 0 ==> let updatedResources = applyModifier r (M (rt, True, n))
               in getResourceAmount updatedResources rt == getResourceAmount r rt

-- END: Modifier tests

-- | A ResourceSet stores 3 Modifiers, one for each Resource
type ResourceSet = (Modifier, Modifier, Modifier)

toString :: ResourceSet -> String
toString (m1, m2, m3) = intercalate ", " $ filter (not . null) [show m1, show m2, show m3]

set0 :: ResourceSet
set0 = (keepFood, keepClothes, keepMoney)

set1 :: ResourceSet
set1 = (M (Food, True, 10), keepClothes, keepMoney)

set2 :: ResourceSet
set2 = (keepFood, M (Clothes, False, 20), keepMoney)

set3 :: ResourceSet
set3 = (keepFood, keepClothes, M (Money, True, 50))

set5 :: ResourceSet
set5 = (M (Food, True, 10), M (Clothes, False, 20), M (Money, True, 50))

-- >>> toString set0
-- ""

-- >>> toString set1
-- "+10 food"

-- >>> toString set2
-- "-20 clothes"

-- >>> toString set3
-- "+50 money"

-- >>> toString set5
-- "+10 food, -20 clothes, +50 money"

-- Event is composed of:
-- 1. String describing the event
-- 2. Set of Resources that the event changes
newtype Event = E { event :: (String, ResourceSet)} deriving (Show, Eq)

-- for example:
event1 :: Event
event1 = E ("You Found A River", riverMod) where
    riverMod = (keepFood, M (Clothes, False, 20), keepMoney)

applyEvent :: Resources s -> Event -> Resources s
applyEvent r e = case e of
    E (s, (m1, m2, m3)) -> r -- updateResources r m1 m2 m3

resourceModifier :: ResourceSet -> ResourceType -> Modifier
resourceModifier (m1, m2, m3) rt = case rt of
    Food -> m1
    Clothes -> m2
    Money -> m3

eventString :: Event -> String
eventString e = case e of
    E (s, _) -> s

eventOutcome :: Event -> ResourceSet
eventOutcome e = case e of
    E (_, rs) -> rs

eventToString :: Event -> String
eventToString (E (s, rs)) = case rs of
    (m1, m2, m3) -> s ++ ": " ++ "+10 food"

-- BEGIN: Event tests

eventTests :: Test
eventTests = TestList
    [ testEventDescription
    , testEventOutcome
    , testEventToString
    ]

eventHunting :: Event
eventHunting = E ("Hunting", (M (Food, True, 10), keepClothes, keepMoney))

testEventDescription :: Test
testEventDescription = TestCase $
    assertEqual "Event description" "Hunting" (eventString eventHunting)

testEventOutcome :: Test
testEventOutcome = TestCase $
    let huntingResources = (M (Food, True, 10), keepClothes, keepMoney) in
    let hunting = applyEvent zeroResources eventHunting in
    assertEqual "Event outcome" hunting (applyEvent zeroResources eventHunting)

testEventToString :: Test
testEventToString = TestCase $
    assertEqual "Event to string" "Hunting: +10 food" (eventToString eventHunting)

-- END: Event tests

-- >>> runTestTT eventTests
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
