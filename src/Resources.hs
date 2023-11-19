module Resources where


-- make functions to change the state generic
-- pass in which resource we would like to change

-- nat 

-- import qualified Data.Nat
import Data.Type.Nat

newtype Resources s = GS { money :: Nat }

manager :: Resources s -> Nat
manager = money

-- >>> manager (GS 100)
-- 100

-- make functions to change the state generic
-- pass in which resource we would like to change


-- nat 
