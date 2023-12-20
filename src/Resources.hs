module Resources where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Type.Nat
import Test.HUnit

-- | Enumerated type for different resource types in the game.
data ResourceType = Food | Clothes | Bullets | Oxen | Medicine | Wheels | Money deriving (Eq, Ord)

-- Custom display for ResourceType.
instance Show ResourceType where
  show Food = "food"
  show Clothes = "clothes"
  show Bullets = "bullets"
  show Oxen = "oxen"
  show Medicine = "medicine"
  show Wheels = "wheels"
  show Money = "money"

-- | Type for mapping a resource to its quantity.
type ResourcesM = Map ResourceType Nat

-- | Structure to hold the quantity of each resource.
data Resources s = Resources
  { food :: Nat,
    clothes :: Nat,
    bullets :: Nat,
    oxen :: Nat,
    medicine :: Nat,
    wheels :: Nat,
    money :: Nat
  }
  deriving (Eq, Ord, Show)

-- | Resource instance with zero quantities for each resource type.
zeroResources :: Resources s
zeroResources = Resources {food = 0, clothes = 0, bullets = 0, oxen = 0, medicine = 0, wheels = 0, money = 0}

-- | Resource instance with initial quantities set for game start.
initialResources :: Resources s
initialResources = Resources {food = 0, clothes = 0, bullets = 0, oxen = 0, medicine = 0, wheels = 0, money = 700}

bankerResources :: Resources s
bankerResources = zeroResources {money = 1000}

carpenterResources :: Resources s
carpenterResources = zeroResources {money = 800}

farmerResources :: Resources s
farmerResources = zeroResources {money = 400}

addMoney :: Resources s -> Nat -> Resources s
addMoney r n = r {money = money r + n}

-- | Function to subtract money from a resource, with error handling for insufficient funds.
substractMoney :: Resources s -> Nat -> Resources s
substractMoney r n =
  case minus (money r) n of
    Just newAmount -> r {money = newAmount}
    Nothing -> error "Insufficient funds"

-- | Function to add a specified amount of a particular resource.
addResources' :: Resources s -> ResourceType -> Nat -> Resources s
addResources' r rtype n = case rtype of
  Food -> r {food = food r + n}
  Clothes -> r {clothes = clothes r + n}
  Bullets -> r {bullets = bullets r + n}
  Oxen -> r {oxen = oxen r + n}
  Medicine -> r {medicine = medicine r + n}
  Wheels -> r {wheels = wheels r + n}
  Money -> r {money = money r + n}

-- | Function to subtract a specified amount of a particular resource.
substractResources' :: Resources s -> ResourceType -> Nat -> Resources s
substractResources' r rtype n = case rtype of
  Food -> r {food = fromMaybe (food r) (food r `minus` n)}
  Clothes -> r {clothes = fromMaybe (clothes r) (clothes r `minus` n)}
  Bullets -> r {bullets = fromMaybe (bullets r) (bullets r `minus` n)}
  Oxen -> r {oxen = fromMaybe (oxen r) (oxen r `minus` n)}
  Medicine -> r {medicine = fromMaybe (medicine r) (medicine r `minus` n)}
  Wheels -> r {wheels = fromMaybe (wheels r) (wheels r `minus` n)}
  Money -> r {money = fromMaybe (money r) (money r `minus` n)}

-- | Utility function to get the quantity of a specific resource.
getResourceAmount :: Resources s -> ResourceType -> Nat
getResourceAmount r rtype = case rtype of
  Food -> food r
  Clothes -> clothes r
  Bullets -> bullets r
  Oxen -> oxen r
  Medicine -> medicine r
  Wheels -> wheels r
  Money -> money r

-- | Utility for subtracting two `Nat` values with handling for negative results.
minus :: Nat -> Nat -> Maybe Nat
minus a b = if sub >= 0 then Just (fromIntegral sub) else Nothing
  where
    sub = fromIntegral a - fromIntegral b :: Int

-- | Utility to convert a `Nat` value to the corresponding `ResourceType`.
natToRes :: Nat -> ResourceType
natToRes 0 = Food
natToRes 1 = Clothes
natToRes 2 = Bullets
natToRes 3 = Oxen
natToRes 4 = Medicine
natToRes 5 = Wheels
natToRes _ = Money

-- | Function to get the cost of a specific resource type.
resCost :: ResourceType -> Nat
resCost Food = 2
resCost Clothes = 15
resCost Bullets = 5
resCost Oxen = 100
resCost Medicine = 25
resCost Wheels = 50
resCost Money = 1

-- | Function to get the shop description for a specific resource type.
shopTile :: ResourceType -> String
shopTile Food = "pounds of food"
shopTile Clothes = "sets of clothing"
shopTile Bullets = "boxes of bullets"
shopTile Oxen = "oxen"
shopTile Medicine = "packs of medicine"
shopTile Wheels = "extra wagon wheels"
shopTile _ = "You can't buy that"

-- | Convert a `Nat` value to its corresponding shop string representation.
shopNatToString :: Nat -> String
shopNatToString n = shopTile (natToRes n)