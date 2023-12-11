module Resources where

-- import qualified Data.Nat
import Data.Type.Nat
import Data.Map (Map)

data ResourceType = Food | Clothes | Money deriving (Eq, Ord)

instance Show ResourceType where
    show Food = "food"
    show Clothes = "clothes"
    show Money = "money"

type ResourcesM = Map ResourceType Nat

data Resources s = Resources {
    food :: Nat,
    clothes :: Nat,
    money :: Nat
} deriving (Eq, Ord, Show)

zeroResources :: Resources s
zeroResources = Resources { food = 0, clothes = 0, money = 0 }

initialResources :: Resources s
initialResources = Resources { food = 0, clothes = 0, money = 700 }

addResources' :: Resources s -> ResourceType -> Nat -> Resources s
addResources' r rtype n = case rtype of
    Food -> r { food = food r + n }
    Clothes -> r { clothes = clothes r + n }
    Money -> r { money = money r + n }

addMoney :: Resources s -> Nat -> Resources s
addMoney r n = r { money = money r + n }

substractMoney :: Resources s -> Nat -> Resources s
substractMoney r n = 
    if n > money r then error "Insufficient funds" 
    else r { money = money r - n }

getResourceAmount :: Resources s -> ResourceType -> Nat
getResourceAmount r rtype = case rtype of
    Food -> food r
    Clothes -> clothes r
    Money -> money r

-- | utility for substraction
minus :: Nat -> Nat -> Maybe Nat
minus a b = if sub >= 0 then Just (fromIntegral sub) else Nothing
    where sub = fromIntegral a - fromIntegral b :: Int