module Resources where

-- make functions to change the state generic
-- pass in which resource we would like to change
-- nat 

-- import qualified Data.Nat
import Data.Type.Nat
import Data.Map (Map)

newtype Resource s = R { currency :: Nat }

manager :: Resource s -> Nat
manager = currency

-- >>> manager (GS 100)
-- 100

initialResource :: Resource s
initialResource = R { currency = 100 }

addResource :: Resource s -> Nat -> Resource s
addResource r n = r { currency = currency r + n }

substractResource :: Resource s -> Nat -> Resource s
substractResource r n = 
    if n > currency r then error "Insufficient funds" 
    else r { currency = currency r - n }

data ResourceType = Food | Clothes | Money

type ResourcesM = Map ResourceType Nat


data Resources s = Resources {
    food :: Nat,
    clothes :: Nat,
    money :: Nat
}


-- data.Map ResourceType -> Nat
-- get
-- simplify, not case

initialResources :: Resources s
initialResources = Resources { food = 0, clothes = 0, money = 800 }

addResources :: Resources s -> ResourceType -> Nat -> Resources s
addResources r rtype n = case rtype of
    Food -> r { food = food r + n }
    Clothes -> r { clothes = clothes r + n }
    Money -> r { money = money r + n }

substractResources :: Resources s -> ResourceType -> Nat -> Resources s
substractResources r rtype n = case rtype of
    Food ->    if n > food r then error "Insufficient food"
               else r { food = food r - n }
    Clothes -> if n > clothes r then error "Insufficient clothes"
               else r { clothes = clothes r - n }
    Money ->   if n > money r then error "Insufficient funds"
               else r { money = money r - n }

addMoney :: Resources s -> Nat -> Resources s
addMoney r n = r { money = money r + n }

substractMoney :: Resources s -> Nat -> Resources s
substractMoney r n = 
    if n > money r then error "Insufficient funds" 
    else r { money = money r - n }

-- make functions to change the state generic
-- pass in which resource we would like to change