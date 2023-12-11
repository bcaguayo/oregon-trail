module GameState where

-- import Resources

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import Data.Type.Nat (Nat)
import Locations (natToDate)
import Options
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
    status :: GameStatus
  }
  deriving (Eq)

targetMileage :: Nat
targetMileage = 1000

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
      ++ natToDate (date gs)
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

-- gs <- S.get
-- return $ mileage gs >= targetMileage
--   || money (resources gs) <= 0
--   || food (resources gs) <= 0
--   || health gs == Critical

-- | handle event
-- handleEventM :: GameStateM ()
-- handleEventM = do
--   gs <- lift S.get
--   case event gs of
--     Disease -> do
--       newResources <- substractResources (resources gs) Food 10
--       lift $ S.put $ gs {health = Ill, resources = newResources}
--     GoodHarvest -> do
--       newResources <- addResources (resources gs) Food 50
--       lift $ S.put $ gs {resources = newResources}
--     BadWeather -> do
--       newResources <- substractResources (resources gs) Food 20
--       lift $ S.put $ gs {resources = newResources}
--     _ -> return ()

-- | handle user input
performActionM :: Command -> GameStateM ()
performActionM command = case command of
  Help -> return () -- help information
  Quit -> return () -- quit game
  Status -> return () -- show status
  Travel -> travelActionM
  Rest -> restActionM
  Pace -> paceActionM
  Shop -> shopActionM

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

paceActionM :: GameStateM ()
paceActionM = lift $ S.modify $ \gs ->
  let updatedPace = case pace gs of
        Slow -> Fast
        Fast -> Slow
   in gs {pace = updatedPace}

-- | return travel distance
travelDistance :: Pace -> Nat
travelDistance Slow = 95 -- slow travel, for example 95  miles each time
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
  let (result, state') = S.runState (runExceptT updateGameStateM) state
  case result of
    Left errMsg -> assertFailure $ "Error during state update: " ++ errMsg
    Right _ -> do
      assertEqual "Date should increase by 1" 2 (date state')
      assertEqual "Mileage should increase by 10 for Slow pace" 110 (mileage state')

testUpdateHealth :: Test
testUpdateHealth = TestCase $ do
  let state = initialGameState {health = Healthy, resources = initialResources {food = 5, clothes = 0, money = 800}}
  let (result, state') = S.runState (runExceptT updateHealthM) state
  case result of
    Left errMsg -> assertFailure $ "Error during health update: " ++ errMsg
    Right _ -> assertEqual "Health should change to Ill if food is low" Ill (health state')

testIsGameEnd :: Test
testIsGameEnd = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end check: " ++ errMsg
    Right end -> assertBool "Game should end when target mileage reached" end

testTravelAction :: Test
testTravelAction = TestCase $ do
  let initial = initialGameState {mileage = 0, pace = Slow}
  let (result, state') = S.runState (runExceptT travelActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during travel action: " ++ errMsg
    Right _ -> assertEqual "Slow travel should increase mileage by 10" 10 (mileage state')

testHuntAction :: Test
testHuntAction = TestCase $ do
  let initial = initialGameState {resources = initialResources {food = 0}}
  let (result, state') = S.runState (runExceptT shopActionM) initial
  case result of
    Left errMsg -> assertFailure $ "Error during hunt action: " ++ errMsg
    Right _ -> assertEqual "Hunting should increase food by 30" 30 (food (resources state'))

testGameEndMileage :: Test
testGameEndMileage = TestCase $ do
  let state = initialGameState {mileage = targetMileage}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end mileage check: " ++ errMsg
    Right end -> assertBool "Game should end when target mileage reached" end

testGameEndHealth :: Test
testGameEndHealth = TestCase $ do
  let state = initialGameState {health = Critical}
  let (result, isEnd) = S.runState (runExceptT isGameEndM) state
  case result of
    Left errMsg -> assertFailure $ "Error during game end health check: " ++ errMsg
    Right end -> assertBool "Game should end when health is Critical" end

testUpdateResources :: Test
testUpdateResources = TestCase $ do
  -- use addResources to get initial resources
  let initialResourcesState = runExceptT $ addResources initialResources Food 10
  let (initialResourcesResult, _) = S.runState initialResourcesState initialGameState
  case initialResourcesResult of
    Left errMsg -> assertFailure $ "Error during initial resource setup: " ++ errMsg
    Right updatedResources -> do
      let initialState = initialGameState {resources = updatedResources}
      let (result, state') = S.runState (runExceptT updateResourcesM) initialState
      case result of
        Left errMsg -> assertFailure $ "Error during resource update: " ++ errMsg
        Right _ -> assertEqual "Food should decrease by 5 after update" 5 (food $ resources state')

-- res10 = addResources initialResources Food 10

-- res5 = substractResources res10 Food 5

-- >>> res10
-- No instance for (Show1 (State GameState))
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a30Hm
-- >>> res5
-- No instance for (Show1 (State GameState))
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a30Mr

tests :: Test
tests =
  TestList
    [ testUpdateGameState,
      testUpdateHealth,
      testIsGameEnd,
      testTravelAction,
      testHuntAction,
      testGameEndMileage,
      testGameEndHealth,
      testUpdateResources
    ]

-- Debug.Trace
-- >>> :k TestCase
-- Not in scope: type constructor or class `TestCase'

-- test1 :: Test
-- test1 = TestCase $ assertBool True

-- runTestTT

-- >>> runTestTT testUpdateResources
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

runTest :: IO ()
runTest = runTestTT tests >>= print

-- resource
newtype Resource s = R {currency :: Nat}

manager :: Resource s -> Nat
manager = currency

-- >>> manager (GS 100)
-- 100

initialResource :: Resource s
initialResource = R {currency = 100}

addResource :: Resource s -> Nat -> Resource s
addResource r n = r {currency = currency r + n}

substractResource :: Resource s -> Nat -> Resource s
substractResource r n =
  if n > currency r
    then error "Insufficient funds"
    else r {currency = currency r - n}

data ResourceType = Food | Clothes | Money deriving (Eq, Ord)

instance Show ResourceType where
  show Food = "food"
  show Clothes = "clothes"
  show Money = "money"

type ResourcesM = Map ResourceType Nat

data Resources s = Resources
  { food :: Nat,
    clothes :: Nat,
    money :: Nat
  }
  deriving (Eq, Ord, Show)

-- data.Map ResourceType -> Nat
-- get
-- simplify, not case

zeroResources :: Resources s
zeroResources = Resources {food = 0, clothes = 0, money = 0}

initialResources :: Resources s
initialResources = Resources {food = 0, clothes = 0, money = 700}

initialResources2 :: Resources s
initialResources2 = Resources {food = 10, clothes = 0, money = 800}

-- make functions to change the state generic
-- pass in which resource we would like to change

addResources :: Resources s -> ResourceType -> Nat -> ExceptT String (S.State GameState) (Resources s)
addResources r rt n = return $ -- assume a permanent success
  case rt of
    Food -> r {food = food r + n}
    Clothes -> r {clothes = clothes r + n}
    Money -> r {money = money r + n}

-- return Left
-- substractResources :: Resources s -> ResourceType -> Nat -> Resources s
-- substractResources r rtype n = case rtype of
--   Food -> case food r `minus` n of
--     Just n' -> r {food = n'}
--     Nothing -> error "Insufficient food"
--   Clothes -> case clothes r `minus` n of
--     Just n' -> r {clothes = n'}
--     Nothing -> error "Insufficient clothes"
--   Money -> case money r `minus` n of
--     Just n' -> r {money = n'}
--     Nothing -> error "Insufficient funds"
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

addMoney :: Resources s -> Nat -> Resources s
addMoney r n = r {money = money r + n}

substractMoney :: Resources s -> Nat -> Resources s
substractMoney r n =
  if n > money r
    then error "Insufficient funds"
    else r {money = money r - n}

getResourceAmount :: Resources s -> ResourceType -> Nat
getResourceAmount r rtype = case rtype of
  Food -> food r
  Clothes -> clothes r
  Money -> money r

-- | utility for substraction
minus :: Nat -> Nat -> Maybe Nat
minus a b = if sub >= 0 then Just (fromIntegral sub) else Nothing
  where
    sub = fromIntegral a - fromIntegral b :: Int
