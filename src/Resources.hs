module Resources where

import Data.Map (Map)
import Data.Type.Nat
import Test.HUnit

data ResourceType = Food | Clothes | Money | Bullets | Oxen | Medicine | Wheels deriving (Eq, Ord)

instance Show ResourceType where
  show Food = "food"
  show Clothes = "clothes"
  show Money = "money"
  show Bullets = "bullets"
  show Oxen = "oxen"
  show Medicine = "medicine"
  show Wheels = "wheels"

type ResourcesM = Map ResourceType Nat

data Resources s = Resources
  { food :: Nat,
    clothes :: Nat,
    money :: Nat,
    bullets :: Nat,
    oxen :: Nat,
    medicine :: Nat,
    wheels :: Nat
  }
  deriving (Eq, Ord, Show)

zeroResources :: Resources s
zeroResources = Resources {food = 0, clothes = 0, money = 0, bullets = 0, oxen = 0, medicine = 0, wheels = 0}

initialResources :: Resources s
initialResources = Resources {food = 0, clothes = 0, money = 700, bullets = 0, oxen = 0, medicine = 0, wheels = 0}

addResources' :: Resources s -> ResourceType -> Nat -> Resources s
addResources' r rtype n = case rtype of
  Food -> r {food = food r + n}
  Clothes -> r {clothes = clothes r + n}
  Money -> r {money = money r + n}
  Bullets -> r {bullets = bullets r + n}
  Oxen -> r {oxen = oxen r + n}
  Medicine -> r {medicine = medicine r + n}
  Wheels -> r {wheels = wheels r + n}

addMoney :: Resources s -> Nat -> Resources s
addMoney r n = r {money = money r + n}

substractMoney :: Resources s -> Nat -> Resources s
substractMoney r n =
  case minus (money r) n of
    Just newAmount -> r {money = newAmount}
    Nothing -> error "Insufficient funds"

getResourceAmount :: Resources s -> ResourceType -> Nat
getResourceAmount r rtype = case rtype of
  Food -> food r
  Clothes -> clothes r
  Money -> money r
  Bullets -> bullets r
  Oxen -> oxen r
  Medicine -> medicine r
  Wheels -> wheels r

-- | utility for substraction
minus :: Nat -> Nat -> Maybe Nat
minus a b = if sub >= 0 then Just (fromIntegral sub) else Nothing
  where
    sub = fromIntegral a - fromIntegral b :: Int

natToRes :: Nat -> ResourceType
natToRes 0 = Food
natToRes 1 = Clothes
natToRes 2 = Bullets
natToRes 3 = Oxen
natToRes 4 = Medicine
natToRes 5 = Wheels
natToRes _ = Money

-- 测试在资金充足时正确减去金额
testSubstractMoneySuccessfully :: Test
testSubstractMoneySuccessfully = TestCase $ do
  let initialResources = Resources {money = 100, clothes = 0, food = 0, bullets = 0, oxen = 0, medicine = 0, wheels = 0} -- 其他资源初始化
  let amountToSubstract = 50
  let updatedResources = substractMoney initialResources amountToSubstract
  assertEqual "Should substract money correctly" 50 (money updatedResources)

tests :: Test
tests = TestList [testSubstractMoneySuccessfully]

-- 执行测试
runTests :: IO ()
runTests = runTestTT tests >>= print