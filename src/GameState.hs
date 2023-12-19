module GameState where

-- import Resources

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Type.Nat (Nat)
import Locations
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
  let visitedLocations = visited gs -- Renamed local variable
  let newLocation = getLocation currentMileage visitedLocations
  unless (newLocation `Set.member` visitedLocations) $ do
    let newVisited = Set.insert newLocation visitedLocations
    lift $ S.put $ gs {visited = newVisited}

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

-- shopActionM' :: ResourceType -> Bool -> Nat -> ExceptT String GameStateM ()
-- shopActionM' res pos amount = undefined
shopActionM' :: ResourceType -> Bool -> Nat -> GameStateM ()
shopActionM' resourceType isBuying amount = do
  gs <- lift S.get -- Get the current game state.
  let currentResources = resources gs

  -- Calculate the cost of the transaction.
  let cost = amount * 5 -- Assuming a unit cost of 5 for all resources.

  -- Perform the resource update within the ExceptT monad.
  updatedResources <-
    if isBuying
      then do
        -- If buying, add the purchased resource.
        let resourcesAfterAddition = addResources' currentResources resourceType amount

        -- Subtract the equivalent amount of money.
        substractResources resourcesAfterAddition Money cost
      else do
        -- Logic for selling resources.
        case minus (getResourceAmount currentResources resourceType) amount of
          Just n' -> return $ addResources' currentResources resourceType n'
          Nothing -> throwError "Insufficient resources"

  -- Apply the updated resources to the game state.
  lift $ S.put $ gs {resources = updatedResources}

-- do
-- gs <- lift S.get
-- -- First, add food resources
-- let updatedResources = if pos
--           then addResources (resources gs) res amount
--           else substractResources (resources gs) res amount
-- lift $ S.put $ gs {resources = updatedResources}

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

getMoney :: GameState -> Int
getMoney gs = fromIntegral (money (resources gs))