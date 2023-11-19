module Resources where

-- import Data.Type.Nat

newtype Resources s = GS { money :: Int }

-- make functions to change the state generic
-- pass in which resource we would like to change

-- nat 