module GameState where

-- import Resources

import Control.Monad.Except
  ( ExceptT (..),
    MonadError (throwError),
    MonadTrans (lift),
    foldM,
    runExceptT,
  )
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Type.Nat (Nat)
import Events (Event (E), Modifier (M), Outcome (O))
import Locations (Location, getLocation, initialVisitedSet, natToDate)
import Options
import Resources
import State as S
import Test.HUnit
  ( State,
    Test (TestCase, TestList),
    assertBool,
    assertEqual,
    assertFailure,
    runTestTT,
  )
import Test.QuickCheck ()
import Text ()

data Pace = Slow | Fast deriving (Show, Eq)

data HealthStatus = Healthy | Ill | Critical deriving (Show, Eq)

data GameStatus = Playing | GameOver | GameEnd deriving (Show, Eq)

data GameState = GameState
  { date :: Nat,
    mileage :: Nat,
    pace :: Pace,
    health :: HealthStatus,
    resources :: Resources ResourceType,
    status :: GameStatus,
    visitedSet :: Set Location
  }
  deriving (Eq)

targetMileage :: Nat
targetMileage = 2170

checkParameters :: Nat -> Nat -> Pace -> Bool
checkParameters date mileage pace =
  validDate date && validMileage mileage -- && validPace pace

type GameStateM = ExceptT String (S.State GameState)

-- ____________________________________________________________________

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
      ++ natToDate (fromIntegral (date gs))
      ++ ", Mileage: "
      ++ show (mileage gs)
      ++ ", Pace: "
      ++ show (pace gs)
      ++ ", Health: "
      ++ show (health gs)
      ++ ", Resources: "
      ++ show (resources gs)
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
      status = Playing,
      visitedSet = initialVisitedSet
    }

checkState :: GameState -> GameState
checkState gs
  | date gs > 266 = gsO
  | mileage gs >= targetMileage = gsE
  | food (resources gs) <= 0 = gsO
  | health gs == Critical = gsO
  | otherwise = gs
  where
    gsE = gs {status = GameEnd}
    gsO = gs {status = GameOver}

{-
1. Use getLocation from Locations Module
2. Pass in the mileage from the state
3. Update the state and add the new location
4. return the location given by getLocation
5. this allows me to get options
-}
-- Pass In Mileage

-- | Visit a new location based on the current mileage and update the visited locations set.
visitNewLocation :: GameStateM ()
visitNewLocation = do
  gs <- lift S.get
  let currentMileage = mileage gs
  let visitedLocations = visitedSet gs -- Renamed local variable
  let newLocation = getLocation currentMileage visitedLocations
  unless (newLocation `Set.member` visitedLocations) $ do
    let newVisited = Set.insert newLocation visitedLocations
    lift $ S.put $ gs {visitedSet = newVisited}

-- | update game state
updateGameStateM :: GameStateM ()
updateGameStateM = do
  gs <- lift S.get -- use lift leverage S.get to ExceptT layer
  let newDate = date gs + 1
      newMileage = case pace gs of
        Slow -> mileage gs + paceSlow
        Fast -> mileage gs + paceFast
  lift $ S.modify $ \s -> s {date = newDate, mileage = newMileage} -- use lift leverage S.modify to ExceptT layer
  updateHealthM
  updateResourcesM

updateHealthM :: GameStateM ()
updateHealthM = lift $ S.modify $ \gs ->
  let newHealth = case health gs of
        Healthy -> if isIllConditionMet gs then Ill else Healthy
        Ill -> if isRecoveryConditionMet gs then Healthy else Ill
        Critical -> if isRecoveryConditionMet gs then Ill else Critical
   in gs {health = newHealth}

-- | check if ill condition is met
isIllConditionMet :: GameState -> Bool
isIllConditionMet gs = food (resources gs) < 10

-- | check if recovery condition is met
isRecoveryConditionMet :: GameState -> Bool
isRecoveryConditionMet gs = food (resources gs) > 50

-- | update resources
-- updateResourcesM :: GameStateM ()
-- updateResourcesM = S.modify $ \gs ->
--   let currentResources = resources gs
--       foodConsumption = 5
--    in gs {resources = substractResources currentResources Food foodConsumption}
updateResourcesM :: GameStateM ()
updateResourcesM = do
  gs <- lift S.get
  let currentResources = resources gs
  let foodConsumption = 5
  newResources <- substractResources currentResources Food foodConsumption
  lift $ S.put $ gs {resources = newResources}

-- | check if game is end
isGameEndM :: GameStateM Bool
isGameEndM = do
  gs <- lift S.get
  return $
    mileage gs >= targetMileage
      || money (resources gs) <= 0
      || food (resources gs) <= 0
      || health gs == Critical

-- | handle user input
performActionM :: Command -> GameStateM ()
performActionM command = case command of
  Help -> return () -- help information
  Quit -> return () -- quit game
  Status -> return () -- show status
  Travel -> travelActionM
  Hunt -> huntActionM
  Rest -> restActionM
  Pace -> paceActionM
  Shop -> shopActionM'

travelActionM :: GameStateM ()
travelActionM = do
  gs <- lift S.get
  let updatedMileage = mileage gs + travelDistance (pace gs)
      updatedDate = date gs + 1
  newResources <- substractResources (resources gs) Food 5
  lift $ S.put $ gs {date = updatedDate, resources = newResources, mileage = updatedMileage}

restActionM :: GameStateM ()
restActionM = lift $ S.modify $ \gs ->
  let improvedHealth = if health gs == Ill then Healthy else health gs
   in gs {health = improvedHealth}

shopActionM' :: GameStateM ()
shopActionM' = do
  gs <- lift S.get
  let gainFood = 50
  let foodCost = 10
  -- First, add food resources
  resourcesAfterAddingFood <- addResources (resources gs) Food gainFood
  -- Then, subtract money resources
  updatedResources <- substractResources resourcesAfterAddingFood Money foodCost
  lift $ S.put $ gs {resources = updatedResources}

paceActionM :: GameStateM ()
paceActionM = lift $ S.modify $ \gs ->
  let updatedPace = case pace gs of
        Slow -> Fast
        Fast -> Slow
   in gs {pace = updatedPace}

huntActionM :: GameStateM ()
huntActionM = do
  gs <- lift S.get
  let gainFood = 30
  resourcesAfterAddingFood <- addResources (resources gs) Food gainFood
  lift $ S.put $ gs {resources = resourcesAfterAddingFood}

-- | return travel distance
travelDistance :: Pace -> Nat
travelDistance Slow = 95 -- slow travel, for example 95  miles each time
travelDistance Fast = 145 -- fast travel, for example 145 miles each time

-- UPDATE
nextDate :: GameState -> GameState
nextDate gs = updateMileage gs {date = date gs + 14}

updateMileage :: GameState -> GameState
updateMileage gs = case pace gs of
  Slow -> gs {mileage = mileage gs + 7}
  Fast -> gs {mileage = mileage gs + 14}

-------------- | Resource Functions

-- Resource Functions
addResources :: Resources s -> ResourceType -> Nat -> ExceptT String (S.State GameState) (Resources s)
addResources r rt n = return $ -- assume a permanent success
  case rt of
    Food -> r {food = food r + n}
    Clothes -> r {clothes = clothes r + n}
    Money -> r {money = money r + n}
    Bullets -> r {bullets = bullets r + n}
    Oxen -> r {oxen = oxen r + n}
    Medicine -> r {medicine = medicine r + n}
    Wheels -> r {wheels = wheels r + n}

substractResources :: Resources s -> ResourceType -> Nat -> ExceptT String (S.State GameState) (Resources s)
substractResources r rtype n = case rtype of
  Food -> case food r `minus` n of
    Just n' -> return $ r {food = n'}
    Nothing -> throwError "Insufficient food"
  Clothes -> case clothes r `minus` n of
    Just n' -> return $ r {clothes = n'}
    Nothing -> throwError "Insufficient clothes"
  Money -> case money r `minus` n of
    Just n' -> return $ r {money = n'}
    Nothing -> throwError "Insufficient funds"
  Bullets -> case bullets r `minus` n of
    Just n' -> return $ r {bullets = n'}
    Nothing -> throwError "Insufficient bullets"
  Oxen -> case oxen r `minus` n of
    Just n' -> return $ r {oxen = n'}
    Nothing -> throwError "Insufficient oxen"
  Medicine -> case medicine r `minus` n of
    Just n' -> return $ r {medicine = n'}
    Nothing -> throwError "Insufficient medicine"
  Wheels -> case wheels r `minus` n of
    Just n' -> return $ r {wheels = n'}
    Nothing -> throwError "Insufficient wheels"

wallet :: GameState -> Int
wallet gs = fromIntegral (money (resources gs))

-------------- | Shop Functions
shopAction :: GameState -> ResourceType -> Nat -> GameState
shopAction gs res amount = do
  let totalCost = amount * 10
      newResources = addResources' (resources gs) res amount
      newResources' = substractResources' newResources Money totalCost
  gs {resources = newResources'}

-------------- | Event Functions
applyModifier :: Resources s -> Modifier -> ExceptT String (S.State GameState.GameState) (Resources s)
applyModifier r (M (rt, b, n)) = if b then addResources r rt n else substractResources r rt n

applyOutcome :: Resources s -> Outcome -> ExceptT String (S.State GameState) (Resources s)
applyOutcome r (O (_, ms)) = foldM applyModifier r ms

applyEvent :: Nat -> Event -> Resources s -> ExceptT String (S.State GameState) (Resources s)
applyEvent option event res = case event of
  E (_, outcomeList) -> applyOutcome res outcome
    where
      outcome = outcomeList !! fromIntegral option

-- case Map.lookup option (eventMap event) of
--   Just (O (_, ms)) -> applyModifiers res ms
--   Nothing -> res

getMoney :: GameState -> Nat
getMoney gs = money (resources gs)
