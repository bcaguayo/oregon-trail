module Locations where

import Data.Type.Nat

-- LOCATIONS
{-
Hits:
Independence, Missouri  0 miles
Missouri River          10 miles
Fort Kearney            325 miles
Fort Laramie            600 miles
Fort Bridger            975 miles
Fort Boise              1500 miles
The Dalles              1780 miles
Oregon City, Oregon     2000 miles
-}

data LocationType = Town | River | Fort | Road deriving (Eq, Ord, Show)

-- can only hunt on the Road
-- can travel always
-- can only rest in a Town and Fort

getOptions :: LocationType -> [String]
getOptions = undefined

locations :: [String]
locations = [ "Independence, Missouri"
            , "Missouri River"
            , "Fort Kearney"
            , "Fort Laramie"
            , "Fort Bridger"
            , "Fort Boise"
            , "The Dalles"
            , "Oregon City, Oregon"
            ]


-- DATES
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

