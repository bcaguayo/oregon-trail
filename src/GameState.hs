module GameState where

-- import Resources

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import Data.Type.Nat (Nat)
import Locations (natToDate)
import Options
import Locations
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
import Data.Set (Set)
import Events (Modifier (M), Event (E), Outcome (O))
import Data.Maybe (fromMaybe)

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
    visited :: Set Location
  }
  deriving (Eq)

targetMileage :: Nat
targetMileage = 2170

checkParameters :: Nat -> Nat -> Pace -> Bool
checkParameters date mileage pace =
  validDate date && validMileage mileage -- && validPace pace

-- type GameStateM = Control.Monad.State.State GameState
-- type GameStateM = S.State GameState
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
      visited = initialVisitedSet
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
visitNewLocation :: GameStateM ()
visitNewLocation = undefined

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
  Shop -> return ()

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

shopActionM :: GameStateM ()
shopActionM = do
  gs <- lift S.get
  let gainFood = 50
  let foodCost = 10
  -- First, add food resources
  resourcesAfterAddingFood <- addResources (resources gs) Food gainFood
  -- Then, subtract money resources
  updatedResources <- substractResources resourcesAfterAddingFood Money foodCost
  lift $ S.put $ gs {resources = updatedResources}

shopActionM' :: ResourceType -> Nat -> ExceptT String GameStateM ()
shopActionM' resourceType amount = do
  gs <- lift S.get -- Get the current game state.
  let currentResources = resources gs

  -- Calculate the cost of the transaction.
  let cost = amount * 5 -- Assuming a unit cost of 5 for all resources.

  -- Update resources based on whether the player is buying or selling.
  updatedResources <- do
        -- If buying, add the purchased resource.
    let resourcesAfterAddition = addResources' currentResources resourceType amount

    -- Try to subtract the equivalent amount of money.
    eitherResult <- runExceptT $ substractResources resourcesAfterAddition Money cost
    case eitherResult of
      Left errMsg -> throwError errMsg -- Throw error if funds are insufficient.
      Right newResources -> return newResources

  -- Update the game state with the new resources.
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

-- update' :: GameState -> ShopCommand -> ((), GameState)
-- update' gs command =
--   case command of
--     Shop resourceType isBuying amount ->
--       let (result, newGs) = runExceptT (shopActionM' resourceType isBuying amount) gs
--        in case result of
--             Left errMsg -> ((), gs) -- handle erroe
--             Right _ -> ((), newGs) -- update game state
--     _ -> ((), gs) -- other command

-- update'' :: Resources a -> GameState -> (Resources a, GameState)
-- update'' = undefined

-- res10 = addResources initialResources Food 10

-- res5 = substractResources res10 Food 5

-- >>> res10

-- >>> res5

-- Debug.Trace
-- >>> :k TestCase
-- Not in scope: type constructor or class `TestCase'

-- test1 :: Test
-- test1 = TestCase $ assertBool True

-- runTestTT

-- >>> runTestTT testUpdateResources
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

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

-- | Shop Functions
{-
If succesful shopping return new game state
If not succesful shopping return error message "Not enought money"
-}
shopping :: ResourceType -> Nat -> GameState -> ExceptT String GameStateM ()
shopping res amount = S.runState (runExceptT (shopActionM' res amount))

-- | Event Functions
applyModifier :: Resources s -> Modifier -> ExceptT String (S.State GameState.GameState) (Resources s)
applyModifier r (M (rt, b, n)) = if b then addResources r rt n else substractResources r rt n

-- applyModifiers :: Resources s -> Set Modifier -> ExceptT String (S.State GameState.GameState) (Resources s)
applyModifiers :: Resources s -> Set Modifier -> ExceptT String (S.State GameState) (Resources s)
applyModifiers = foldM applyModifier

applyOutcome :: Resources s -> Outcome -> Resources s
applyOutcome r (O (_, ms)) = applyModifiers r ms

applyEvent :: Nat -> Event -> Resources s -> Resources s
applyEvent option event res = case event of
  E (_, outcomeList) -> applyOutcome res outcome where
    outcome = outcomeList !! fromIntegral option
  -- case Map.lookup option (eventMap event) of
  --   Just (O (_, ms)) -> applyModifiers res ms
  --   Nothing -> res
