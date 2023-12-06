module GameState where

import Control.Monad.State
import State as S
import Data.Map (Map)
import Data.Type.Nat ( Nat )
import Resources
import Test.HUnit
    ( assertBool, assertEqual, runTestTT, Test(TestList, TestCase) )
import Test.QuickCheck ()
import Text ()
import Options
import Locations ( natToDate )

data Pace = Slow | Fast deriving (Show, Eq)

data HealthStatus = Healthy | Ill | Critical deriving (Show, Eq)

data Event = None | Disease | BadWeather | GoodHarvest deriving (Show, Eq)

data GameStatus = Playing | GameOver | GameEnd deriving (Show, Eq)

data GameState = GameState
  { date :: Nat,
    mileage :: Nat,
    pace :: Pace,
    health :: HealthStatus,
    resources :: Resources ResourceType,
    event :: Event,
    status :: GameStatus
  } deriving (Eq)

{-
The exact distance traveled on the Oregon Trail varied depending on the specific 
route taken and the number of detours or side trips made. 
However, the average distance from Independence, Missouri, to Oregon City, 
Oregon, was approximately 2,170 miles (3,490 kilometers). 
This distance could be as short as 2,000 miles (3,200 kilometers) or as long as 
2,500 miles (4,000 kilometers), depending on the route.

Missouri River to Fort Kearney: 325 miles (523 kilometers)
Fort Kearney to Fort Laramie: 250 miles (402 kilometers)
Fort Laramie to Fort Bridger: 400 miles (644 kilometers)
Fort Bridger to Fort Boise: 580 miles (933 kilometers)
Fort Boise to The Dalles: 315 miles (507 kilometers)
The Dalles to Oregon City: 300 miles (483 kilometers)

-}

targetMileage :: Nat
targetMileage = 1000

checkParameters :: Nat -> Nat -> Pace -> Bool
checkParameters date mileage pace =
  validDate date && validMileage mileage -- && validPace pace

-- type GameStateM = Control.Monad.State.State GameState
type GameStateM = S.State GameState

{-
WIP, move Dates code to Events.hs or smth
min Date is March 29 (1)
max Date is December 20 (266)

intermediate Dates are every 14 days
April 12 (15)
April 26 (29)
May 10 (43)
May 24 (57)
June 7 (71)
June 21 (85)
July 5 (99)
July 19 (113)
August 2 (127)
August 16 (141)
August 30 (155)
September 13 (169)
September 27 (183)
October 11 (197)

October 25 (211)
November 8 (225)
November 22 (239)
December 6 (253)
December 20 (266)

without events there are 20 intermediate dates
which means 20 updates/steps

Pace Fast should reach 2000 miles in 200 days
so 14 steps, that's 2000 / 14 = 142.85714285714286 miles per step
let's do 145 miles for fast

Pace Slow shouldn't reach Oregon in time
so 20 * pace < 2000,
pace < 100 miles per step
lets do 95 miles for slow

-}

validDate :: Nat -> Bool
validDate date = date >= 1 && date <= 266

validMileage :: Nat -> Bool
validMileage mileage = mileage >= 0

paceFast :: Nat
paceFast = 145

paceSlow :: Nat
paceSlow = 95

instance Show GameState where
  show gs =
    "Status: { Date: "
      ++ natToDate (date gs)
      ++ ", Mileage: "
      ++ show (mileage gs)
      ++ ", Pace: "
      ++ show (pace gs)
      ++ ", Health: "
      ++ show (health gs)
      ++ ", Resources: "
      ++ show (resources gs)
      ++ ", Event: "
      ++ show (event gs)
      ++ "\n"

-- START=
initialGameState :: GameState
initialGameState =
  GameState
    { date = 1,
      mileage = 0,
      pace = Slow,
      health = Healthy,
      resources = initialResources,
      event = None,
      status = Playing
    }

checkState :: GameState -> GameStatus
checkState gs
  | date gs > 266 = GameOver
  | mileage gs >= targetMileage = GameEnd
  | food (resources gs) <= 0 = GameOver
  | health gs == Critical = GameOver
  | otherwise = Playing

-- | update game state
updateGameStateM :: GameStateM ()
updateGameStateM = do
  gs <- S.get
  let newDate = date gs + 1
      newMileage = case pace gs of
        Slow -> mileage gs + paceSlow     -- 95
        Fast -> mileage gs + paceFast     -- 145
  S.modify $ \s -> s {date = newDate, mileage = newMileage}
  updateHealthM
  updateResourcesM

updateHealthM :: GameStateM ()
updateHealthM = S.modify $ \gs ->
  let newHealth = case health gs of
        Healthy -> if isIllConditionMet gs then Ill else Healthy
        Ill -> if isRecoveryConditionMet gs then Healthy else Ill
        Critical -> if isRecoveryConditionMet gs then Ill else Critical
   in gs {health = newHealth}

-- | check if ill condition is met
isIllConditionMet :: GameState -> Bool
isIllConditionMet gs = food (resources gs) < 10 || event gs == Disease

-- | check if recovery condition is met
isRecoveryConditionMet :: GameState -> Bool
isRecoveryConditionMet gs = food (resources gs) > 50

-- | update resources
updateResourcesM :: GameStateM ()
updateResourcesM = S.modify $ \gs ->
  let currentResources = resources gs
      foodConsumption = 5
   in gs {resources = substractResources currentResources Food foodConsumption}

-- | check if game is end
isGameEndM :: GameStateM Bool
isGameEndM = do
  gs <- S.get
  return $ mileage gs >= targetMileage
    || money (resources gs) <= 0
    || food (resources gs) <= 0
    || health gs == Critical
  -- gs <- S.get
  -- return $ mileage gs >= targetMileage
  --   || money (resources gs) <= 0
  --   || food (resources gs) <= 0
  --   || health gs == Critical

-- | handle event
handleEventM :: GameStateM ()
handleEventM = S.modify $ \gs ->
  case event gs of
    Disease -> gs {health = Ill, resources = substractResources (resources gs) Food 10}
    GoodHarvest -> gs {resources = addResources (resources gs) Food 50}
    BadWeather -> gs {resources = substractResources (resources gs) Food 20}
    _ -> gs

-- | handle user input
performActionM :: Command -> GameStateM ()
performActionM command = case command of
  Help -> return () -- help information
  Quit -> return () -- quit game
  Status -> return () -- show status
  Travel -> travelActionM
  Rest -> paceActionM
  Shop -> shopActionM

travelActionM, restActionM, shopActionM :: GameStateM ()
travelActionM = S.modify $ \gs ->
  let updatedResources = substractResources (resources gs) Food 5
      updatedMileage = mileage gs + travelDistance (pace gs)
      updatedDate = date gs + 1
  in gs {date = updatedDate, resources = updatedResources, mileage = updatedMileage}
restActionM = S.modify $ \gs ->
  let improvedHealth = if health gs == Ill then Healthy else health gs
   in gs {health = improvedHealth}
shopActionM = S.modify $ \gs ->
  let gainFood = 50
      foodCost = 10
      updatedResources = addResources (resources gs) Food gainFood 
      updatedResources' = substractResources updatedResources Money foodCost
  in gs {resources = updatedResources'}

paceActionM :: GameStateM ()
paceActionM = S.modify $ \gs ->
  let updatedPace = case pace gs of
        Slow -> Fast
        Fast -> Slow
  in gs {pace = updatedPace}

-- | return travel distance
travelDistance :: Pace -> Nat
travelDistance Slow = 95  -- slow travel, for example 95  miles each time
travelDistance Fast = 145 -- fast travel, for example 145 miles each time

updateMileage :: GameState -> GameState
updateMileage gs = case pace gs of
  Slow -> gs {mileage = mileage gs + 7}
  Fast -> gs {mileage = mileage gs + 14}

-- UPDATE
update :: GameState -> GameState
update gs = updateMileage gs {date = date gs + 1}

update' :: GameState -> ((), GameState)
update' = undefined

update'' :: Resources a -> GameState -> (Resources a, GameState)
update'' = undefined

testUpdateGameState :: Test
testUpdateGameState = TestCase $ do
  let state = initialGameState {date = 1, mileage = 100, pace = Slow}
  let state' = S.execState updateGameStateM state
  assertEqual "Date should increase by 1" 2 (date state')
  assertEqual "Mileage should increase by 10 for Slow pace" 110 (mileage state')

testUpdateHealth :: Test
testUpdateHealth = TestCase $ do
  let state = initialGameState {health = Healthy, resources = initialResources {food = 5, clothes = 0, money = 800}}
  let state' = S.execState updateHealthM state
  assertEqual "Health should change to Ill if food is low" Ill (health state')

testIsGameEnd :: Test
testIsGameEnd = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let isEnd = S.evalState isGameEndM state
  assertBool "Game should end when target mileage reached" isEnd

testTravelAction :: Test
testTravelAction = TestCase $ do
  let initial = initialGameState {mileage = 0, pace = Slow}
  let state' = S.execState travelActionM initial
  assertEqual "Slow travel should increase mileage by 10" 10 (mileage state')

testHuntAction :: Test
testHuntAction = TestCase $ do
  let initial = initialGameState {resources = initialResources {food = 0}}
  let state' = S.execState shopActionM initial
  assertEqual "Hunting should increase food by 30" 30 (food (resources state'))

testGameEndMileage :: Test
testGameEndMileage = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let isEnd = S.evalState isGameEndM state
  assertBool "Game should end when target mileage reached" isEnd

testGameEndHealth :: Test
testGameEndHealth = TestCase $ do
  let state = initialGameState {health = Critical}
  let isEnd = S.evalState isGameEndM state
  assertBool "Game should end when health is Critical" isEnd

testUpdateResources :: Test
testUpdateResources = TestCase $ do
  let initial = initialGameState {resources = addResources initialResources Food 10}
  let state = S.execState updateResourcesM initial  
  assertEqual "Food should decrease by 5 after update" 5 (food (resources state))

res10 = addResources initialResources Food 10

res5 = substractResources res10 Food 5

-- >>> res10
-- Resources {food = 10, clothes = 0, money = 800}
-- >>> res5
-- Insufficient food

tests :: Test
tests = TestList [testUpdateGameState, testUpdateHealth, testIsGameEnd, testTravelAction, 
                  testHuntAction, testGameEndMileage, testGameEndHealth, testUpdateResources]

-- Debug.Trace
-- >>> :k TestCase
-- Not in scope: type constructor or class `TestCase'

-- test1 :: Test
-- test1 = TestCase $ assertBool True

-- runTestTT

-- >>> runTestTT testUpdateResources
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

main :: IO ()
main = runTestTT tests >>= print