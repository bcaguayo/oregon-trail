module GameState where

import Data.Map (Map)
import Data.Type.Nat
import Resources
import Test.HUnit
import Test.QuickCheck
import Text
import UserCommands

data Pace = Slow | Fast deriving (Show, Eq)

data HealthStatus = Healthy | Ill | Critical deriving (Show, Eq)

data Event = None | Disease | BadWeather | GoodHarvest deriving (Show, Eq)

data GameState = GameState
  { date :: Nat,
    mileage :: Nat,
    pace :: Pace,
    health :: HealthStatus,
    resources :: Resources ResourceType,
    event :: Event
  }

targetMileage :: Nat
targetMileage = 2000

checkParameters :: Nat -> Nat -> Pace -> Bool
checkParameters date mileage pace =
  validDate date && validMileage mileage -- && validPace pace

{-
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
-}

validDate :: Nat -> Bool
validDate date = date >= 1 && date <= 266

validMileage :: Nat -> Bool
validMileage mileage = mileage >= 0

-- validPace :: Pace -> Bool
-- validPace pace = case pace of
--     Slow -> True
--     Fast -> True

-- START=
initialGameState :: GameState
initialGameState =
  GameState
    { date = 1,
      mileage = 0,
      pace = Slow,
      health = Healthy,
      resources = initialResources,
      event = None
    }

-- | update game state
updateGameState :: GameState -> GameState
updateGameState gs =
  let newDate = date gs + 1
      newMileage = case pace gs of
        Slow -> mileage gs + 10
        Fast -> mileage gs + 20
      newHealth = updateHealth gs
      newResources = updateResources gs
   in gs {date = newDate, mileage = newMileage, health = newHealth, resources = newResources}

updateHealth :: GameState -> HealthStatus
updateHealth gs =
  case health gs of
    Healthy -> if isIllConditionMet gs then Ill else Healthy
    Ill -> if isRecoveryConditionMet gs then Healthy else Ill
    Critical -> if isRecoveryConditionMet gs then Ill else Critical
  where
    -- "Conditions of illness" are defined here, such as a shortage of resources or the occurrence of a specific event
    isIllConditionMet gs' = food (resources gs') < 10 || event gs' == Disease

    -- "Conditions for rehabilitation" are defined here, such as adequate resources
    isRecoveryConditionMet gs' = food (resources gs') > 50

-- | update resources
updateResources :: GameState -> Resources ResourceType
updateResources gs =
  let currentResources = resources gs
      -- assume that each person consumes 5 food per day
      foodConsumption = 5
   in substractResources currentResources Food foodConsumption

-- | check if game is end
isGameEnd :: GameState -> Bool
isGameEnd gs =
  mileage gs >= targetMileage
    || money (resources gs) <= 0
    || food (resources gs) <= 0
    || health gs == Critical

-- | handle event
handleEvent :: GameState -> GameState
handleEvent gs =
  case event gs of
    Disease -> gs {health = Ill, resources = substractResources (resources gs) Food 10}
    GoodHarvest -> gs {resources = addResources (resources gs) Food 50}
    BadWeather -> gs {resources = substractResources (resources gs) Food 20}
    _ -> gs

-- | handle user input
performAction :: Command -> GameState -> GameState
performAction command gs =
  case command of
    Help -> gs -- help information
    Quit -> gs -- quit game
    Status -> gs -- show the status of the game
    Travel -> travelAction gs -- travel
    Rest -> restAction gs -- rest
    Hunt -> huntAction gs -- hunt

-- | travel action
travelAction :: GameState -> GameState
travelAction gs =
  let updatedMileage = mileage gs + travelDistance (pace gs)
   in gs {mileage = updatedMileage}

-- | pace to travel distance
travelDistance :: Pace -> Nat
travelDistance Slow = 10 -- assume slow travel increases 10 miles each time
travelDistance Fast = 20 -- assume fast travel increases 20 miles each time

-- | logic of rest action
restAction :: GameState -> GameState
restAction gs =
  let improvedHealth = if health gs == Ill then Healthy else health gs
   in gs {health = improvedHealth}

-- | logic of hunt action
huntAction :: GameState -> GameState
huntAction gs =
  let gainFood = 30 -- assume that hunting can gain 30 food
      updatedResources = addResources (resources gs) Food gainFood
   in gs {resources = updatedResources}

shopAction :: GameState -> GameState
shopAction gs =
  let cost = 20
   in gs {resources = substractMoney (resources gs) cost}

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

-- call text
--
