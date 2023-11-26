module GameState where

import Data.Type.Nat
import Text
import Resources

data Pace = Slow | Fast

data GameState = GameState { date :: Nat, mileage :: Nat, pace :: Pace }

checkParameters :: Nat -> Nat -> Pace -> Bool
checkParameters date mileage pace =
    validDate date && validMileage mileage -- && validPace pace

{-
min Date is March 29 (1)
max Date is December 20 (266)

intermediate Dates are every 14 days
April 12 (15)
April 26 (29)
May 10 (43)
May 24 (57)
June 7 (71)
June 21 (85)
July 5 (99)
July 19 (113)
August 2 (127)
August 16 (141)
August 30 (155)
September 13 (169)
September 27 (183)
October 11 (197)
October 25 (211)
November 8 (225)
November 22 (239)
December 6 (253)
December 20 (266)
-} 

validDate :: Nat -> Bool
validDate date = date >= 1 && date <= 266

validMileage :: Nat -> Bool
validMileage mileage = mileage >= 0

-- validPace :: Pace -> Bool
-- validPace pace = case pace of
--     Slow -> True
--     Fast -> True

-- START=
initialGameState :: GameState
initialGameState = GameState { date = 1, mileage = 0, pace = Slow }

updateMileage :: GameState -> GameState
updateMileage gs = case pace gs of
    Slow -> gs { mileage = mileage gs + 7 }
    Fast -> gs { mileage = mileage gs + 14 }

{-
March starts 29th (1) and ends 31st (3)
April starts 1st (4) and ends 30th (34)
May starts 1st (35) and ends 31st (65)
June starts 1st (66) and ends 30th (96)
July starts 1st (97) and ends 31st (127)
August starts 1st (128) and ends 31st (158)
September starts 1st (159) and ends 30th (189)
October starts 1st (190) and ends 31st (220)
November starts 1st (221) and ends 30th (251)
December starts 1st (252) and ends 20th (266)
-}

natToDate :: Nat -> String
natToDate = numtoDate . natToTouple

natToTouple :: Nat -> (Nat, Nat)
natToTouple n
    | n >= 1 && n <= 3 = (3, n)
    | n >= 4 && n <= 34 = (4, n - 3)
    | n >= 35 && n <= 65 = (5, n - 34)
    | n >= 66 && n <= 96 = (6, n - 65)
    | n >= 97 && n <= 127 = (7, n - 96)
    | n >= 128 && n <= 158 = (8, n - 127)
    | n >= 159 && n <= 189 = (9, n - 158)
    | n >= 190 && n <= 220 = (10, n - 189)
    | n >= 221 && n <= 251 = (11, n - 220)
    | n >= 252 && n <= 266 = (12, n - 251)
    | otherwise = error "Invalid date"

numtoDate :: (Nat, Nat) -> String
numtoDate (month, day) = case month of
    3 -> "March " ++ show day
    4 -> "April " ++ show day
    5 -> "May " ++ show day
    6 -> "June " ++ show day
    7 -> "July " ++ show day
    8 -> "August " ++ show day
    9 -> "September " ++ show day
    10 -> "October " ++ show day
    11 -> "November " ++ show day
    12 -> "December " ++ show day
    _ -> error "Invalid date"

-- UPDATE
update :: GameState -> GameState
update gs = updateMileage gs { date = date gs + 1 }

update' :: GameState -> ((), GameState)
update' = undefined

update'' :: Resources a -> GameState -> (Resources a, GameState)
update'' = undefined



-- call text
-- 