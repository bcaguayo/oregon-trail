module Locations where

import Data.Type.Nat
import Text as T
import Options

-- LOCATIONS

data LocationType = Town  | River | Fort | Road deriving (Eq, Ord, Show)

data Location = 
    Independence 
  | MissouriRiver 
  | FortKearney
  | FortLaramie
  | FortBridger
  | FortBoise
  | TheDalles
  | OregonCity
  | Roadside
  deriving (Eq, Ord, Show)

-- can only hunt on the Road
-- can travel always
-- can only rest in a Town and Fort

getOptions :: LocationType -> [Command]
getOptions Road = [Help, Status, Pace, Travel, Hunt, Quit]
getOptions Town = [Help, Status, Travel, Rest, Shop, Quit]
getOptions Fort = [Help, Status, Travel, Rest, Quit]
getOptions River = [Help, Status, Travel, Quit]

-- locationsToOptions :: String -> [Command]

printLocation :: Location -> String
printLocation Independence = "Independence, Missouri"
printLocation MissouriRiver = "Missouri River"
printLocation FortKearney = "Fort Kearney"
printLocation FortLaramie = "Fort Laramie"
printLocation FortBridger = "Fort Bridger"
printLocation FortBoise = "Fort Boise"
printLocation TheDalles = "The Dalles"
printLocation OregonCity = "Oregon City, Oregon"
printLocation Roadside = "The Road"

getLocation :: Nat -> String
getLocation n
  | n >= 0 && n < 10 = "Independence, Missouri"
  | n >= 10 && n < 325 = "Missouri River"
  | n >= 325 && n < 600 = "Fort Kearney"
  | n >= 600 && n < 975 = "Fort Laramie"
  | n >= 975 && n < 1500 = "Fort Bridger"
  | n >= 1500 && n < 1800 = "Fort Boise"
  | n >= 1880 = "The Dalles"
  | otherwise = error "Invalid location"

initialLocationMap :: [(Location, Bool)]
initialLocationMap = [(Independence, False), (MissouriRiver, False), 
  (FortKearney, False), (FortLaramie, False), (FortBridger, False), 
  (FortBoise, False), (TheDalles, False), (OregonCity, False)]

checkLocation :: [(Location, Bool)] -> Location
checkLocation [] = Roadside
checkLocation ((loc, visited):rest) = if visited then checkLocation rest else loc

-- DATES
{-
March starts 29th (1) and ends 31st (3)
April starts 1st (4) and ends 30th (34)
May starts 1st (35) and ends 31st (65)
June starts 1st (66) and ends 30th (96)
July starts 1st (97) and ends 31st (127)
August starts 1st (128) and ends 31st (158)
September ^starts 1st (159) and ends 30th (189)
October star^ts 1st (190) and ends 31st (220)
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

getDate :: Int -> String
getDate i = T.dates !! i



{-
WIP, move Dates code to Events.hs or smth
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

without events there are 20 intermediate dates
which means 20 updates/steps

Pace Fast should reach 2000 miles in 200 days
so 14 steps, that's 2000 / 14 = 142.85714285714286 miles per step
let's do 145 miles for fast

Pace Slow shouldn't reach Oregon in time
so 20 * pace < 2000,
pace < 100 miles per step
lets do 95 miles for slow

Multiples of 145 until 2040
145, 290, 435, 580, 725, 870, 1015, 1160, 1305, 1450, 1595, 1740, 1885, 2030

Multiples of 95 until 2040
95, 190, 285, 380, 475, 570, 665, 760, 855, 950, 1045, 1140, 1235, 1330, 1425, 1520, 1615, 1710, 1805, 1900, 1995

-}

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

{-
The exact distance traveled on the Oregon Trail varied depending on the specific 
route taken and the number of detours or side trips made. 
However, the average distance from Independence, Missouri, to Oregon City, 
Oregon, was approximately 2,170 miles (3,490 kilometers). 
This distance could be as short as 2,000 miles (3,200 kilometers) or as long as 
2,500 miles (4,000 kilometers), depending on the route.

Missouri River to Fort Kearney: 325 miles (523 kilometers)
Fort Kearney to Fort Laramie: 250 miles (402 kilometers)
Fort Laramie to Fort Bridger: 400 miles (644 kilometers)
Fort Bridger to Fort Boise: 580 miles (933 kilometers)
Fort Boise to The Dalles: 315 miles (507 kilometers)
The Dalles to Oregon City: 300 miles (483 kilometers)

-}