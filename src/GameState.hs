module GameState where

import Control.Monad.State
import Data.Map (Map)
import Data.Type.Nat ( Nat )
import Resources
import Test.HUnit
    ( assertBool, assertEqual, runTestTT, Test(TestList, TestCase) )
import Test.QuickCheck ()
import Text ()
import Options

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
targetMileage = 2000

checkParameters :: Nat -> Nat -> Pace -> Bool
checkParameters date mileage pace =
  validDate date && validMileage mileage -- && validPace pace

type GameStateM = Control.Monad.State.State GameState

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
  gs <- get
  let newDate = date gs + 1
      newMileage = case pace gs of
        Slow -> mileage gs + paceSlow     -- 95
        Fast -> mileage gs + paceFast     -- 145
  modify $ \s -> s {date = newDate, mileage = newMileage}
  updateHealthM
  updateResourcesM

updateHealthM :: GameStateM ()
updateHealthM = modify $ \gs ->
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
updateResourcesM = modify $ \gs ->
  let currentResources = resources gs
      foodConsumption = 5
   in gs {resources = substractResources currentResources Food foodConsumption}

-- | check if game is end
isGameEndM :: GameStateM Bool
isGameEndM = gets $ \gs ->
  mileage gs >= targetMileage
    || money (resources gs) <= 0
    || food (resources gs) <= 0
    || health gs == Critical

-- | handle event
handleEventM :: GameStateM ()
handleEventM = modify $ \gs ->
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
  Rest -> restActionM
  Shop -> shopActionM

travelActionM, restActionM, shopActionM :: GameStateM ()
travelActionM = modify $ \gs ->
  let updatedMileage = mileage gs + travelDistance (pace gs)
   in gs {mileage = updatedMileage}
restActionM = modify $ \gs ->
  let improvedHealth = if health gs == Ill then Healthy else health gs
   in gs {health = improvedHealth}
shopActionM = modify $ \gs ->
  let gainFood = 30
      updatedResources = addResources (resources gs) Food gainFood
   in gs {resources = updatedResources}

-- | return travel distance
travelDistance :: Pace -> Nat
travelDistance Slow = 10 -- slow travel, for example 10 miles each time
travelDistance Fast = 20 -- fast travel, for example 20 miles each time

updateMileage :: GameState -> GameState
updateMileage gs = case pace gs of
  Slow -> gs {mileage = mileage gs + 7}
  Fast -> gs {mileage = mileage gs + 14}

{-
March starts 29th (1) and ends 31st (3)
April starts 1st (4) and ends 30th (34)
May starts 1st (35) and ends 31st (65)
June starts 1st (66) and ends 30th (96)
July starts 1st (97) and ends 31st (127)
August starts 1st (128) and ends 31st (158)
September starts 1st (159) and ends 30th (189)
October starts 1st (190) and ends 31st (220)
November starts 1st (221) and ends 30th (251)
December starts 1st (252) and ends 20th (266)
-}

natToDate :: Nat -> String
natToDate = numtoDate . natToTouple

natToTouple :: Nat -> (Nat, Nat)
natToTouple n
  | n >= 1 && n <= 3 = (3, n)
  | n >= 4 && n <= 34 = (4, n - 3)
  | n >= 35 && n <= 65 = (5, n - 34)
  | n >= 66 && n <= 96 = (6, n - 65)
  | n >= 97 && n <= 127 = (7, n - 96)
  | n >= 128 && n <= 158 = (8, n - 127)
  | n >= 159 && n <= 189 = (9, n - 158)
  | n >= 190 && n <= 220 = (10, n - 189)
  | n >= 221 && n <= 251 = (11, n - 220)
  | n >= 252 && n <= 266 = (12, n - 251)
  | otherwise = error "Invalid date"

numtoDate :: (Nat, Nat) -> String
numtoDate (month, day) = case month of
  3 -> "March " ++ show day
  4 -> "April " ++ show day
  5 -> "May " ++ show day
  6 -> "June " ++ show day
  7 -> "July " ++ show day
  8 -> "August " ++ show day
  9 -> "September " ++ show day
  10 -> "October " ++ show day
  11 -> "November " ++ show day
  12 -> "December " ++ show day
  _ -> error "Invalid date"

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
  let state' = execState updateGameStateM state
  assertEqual "Date should increase by 1" 2 (date state')
  assertEqual "Mileage should increase by 10 for Slow pace" 110 (mileage state')

testUpdateHealth :: Test
testUpdateHealth = TestCase $ do
  let state = initialGameState {health = Healthy, resources = initialResources {food = 5, clothes = 0, money = 800}}
  let state' = execState updateHealthM state
  assertEqual "Health should change to Ill if food is low" Ill (health state')

testIsGameEnd :: Test
testIsGameEnd = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let isEnd = evalState isGameEndM state
  assertBool "Game should end when target mileage reached" isEnd

testTravelAction :: Test
testTravelAction = TestCase $ do
  let initial = initialGameState {mileage = 0, pace = Slow}
  let state' = execState travelActionM initial
  assertEqual "Slow travel should increase mileage by 10" 10 (mileage state')

testHuntAction :: Test
testHuntAction = TestCase $ do
  let initial = initialGameState {resources = initialResources {food = 0}}
  let state' = execState shopActionM initial
  assertEqual "Hunting should increase food by 30" 30 (food (resources state'))

testGameEndMileage :: Test
testGameEndMileage = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let isEnd = evalState isGameEndM state
  assertBool "Game should end when target mileage reached" isEnd

testGameEndHealth :: Test
testGameEndHealth = TestCase $ do
  let state = initialGameState {health = Critical}
  let isEnd = evalState isGameEndM state
  assertBool "Game should end when health is Critical" isEnd

testUpdateResources :: Test
testUpdateResources = TestCase $ do
  let initial = initialGameState {resources = addResources initialResources Food 10}
  let state = execState updateResourcesM initial  
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