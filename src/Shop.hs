module Shop where

import Options
import Text
import qualified Control.Monad.State as S

data ShopType = SFood | SClothes | SOxen

itemToString :: ShopType -> String
itemToString s = case s of
  SOxen -> "YOUR OXEN TEAM"
  SFood -> "FOOD"
  SClothes -> "CLOTHES"

-- shop = Shop.shopping SFood 700

-- shopping :: ShopType -> Int -> IO ()
-- shopping cat wallet = do
--   putStrLn (spend cat)
--   input <- getLine
--   case parseInt input of
--     Just x -> if x > 0 && x < wallet then
--                 let newGameState = S.execState (performActionM Travel) gs in
--                 putStrLn "Traveling... \n" >> shopping newGameState
--               else putStrLn "Invalid amount, try again" >> shopping 
--       putStrLn ("YOU HAVE BOUGHT " ++ show x ++ " POUNDS OF FOOD\n")
--     Nothing -> putStrLn "Invalid command, try again" >> shopping

-- shopFood :: IO ()
-- shopFood = do
--   putStrLn food
--   input <- getLine
--   case parseInt input of
--     Just x -> if x > 0 && x < wallet then
--                 let newGameState = S.execState (performActionM Travel) gs in
--                 putStrLn "Traveling... \n" >> shopping newGameState
--               else putStrLn "Invalid amount, try again" >> shopping 
--       putStrLn ("YOU HAVE BOUGHT " ++ show x ++ " POUNDS OF FOOD\n")
--     Nothing -> putStrLn "Invalid command, try again" >> shopping

-- Text Utils _________________________________________________________________


spend s = "HOW MUCH DO YOU WANT TO SPEND ON " ++ itemToString s ++ " ?\n"

oxen = "  EACH OX COSTS $50. \n" ++
           "  YOU NEED AT LEAST 2 OXEN TO PULL THE WAGON.\n"

food = "  THE MINIMUM YOU SHOULD PURCHASE IS 200 POUNDS.\n" ++
           "  THE COST IS $0.20 PER POUND\n"

clothes = "  IT IS A GOOD IDEA TO HAVE AT LEAST TWO SETS OF CLOTHING\n" ++
              "  THE COST IS $10.00 PER SET\n"