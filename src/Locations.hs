module Locations where

import Data.Type.Nat
import Text as T
import Options
import qualified Data.Time.Calendar as Cal
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import State as S

-- LOCATIONS
data LocationType = Town | Road deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord)

getOptions :: LocationType -> [Command]
getOptions Road = [Help, Status, Travel, Pace, Hunt, Quit]
getOptions Town = [Help, Status, Travel, Rest, Shop, Quit]

-- locationsToOptions :: String -> [Command]

instance Show Location where
  show Independence = "Independence, Missouri"
  show MissouriRiver = "Missouri River"
  show FortKearney = "Fort Kearney"
  show FortLaramie = "Fort Laramie"
  show FortBridger = "Fort Bridger"
  show FortBoise = "Fort Boise"
  show TheDalles = "The Dalles"
  show OregonCity = "Oregon City, Oregon"
  show Roadside = "The Road"

locationFromRange :: Nat -> Location
locationFromRange n
  | n >= 0 && n < 10 = Independence
  | n >= 10 && n < 325 = MissouriRiver
  | n >= 325 && n < 600 = FortKearney
  | n >= 600 && n < 975 = FortLaramie
  | n >= 975 && n < 1500 = FortBridger
  | n >= 1500 && n < 1800 = FortBoise
  | n >= 1880 = TheDalles
  | n >= 2170 = OregonCity
  | otherwise = Roadside

initialVisitedSet :: Set Location
initialVisitedSet = Set.fromList [Independence]

-- | Returns the next location and the updated location map
-- getLocation :: Nat -> Set Location -> (Location, Set Location)
-- getLocation n locs = if locationFromRange n `notElem` locs
--   then (locationFromRange n, Set.insert (locationFromRange n) locs)
--   else (Roadside, locs)

getLocation :: Nat -> Set Location -> Location
getLocation n locs = if locationFromRange n `notElem` locs
  then locationFromRange n
  else Roadside

-- >>> getLocation 0 Set.empty
-- (Independence, Missouri,fromList [Independence, Missouri])

-- DATES

showDate :: Int -> String
showDate i = line ++ weekday ++ " " ++ day ++ "\n" ++ line where
  line = "==================================================\n"
  weekday = weekdays !! fromIntegral (weekdayOffset i)
  day = natToDate i

-- >>> showDate 266

natToDate :: Int -> String
natToDate =  dateToStr . natToTouple

natToTouple :: Int -> (Int, Int)
natToTouple n
  | n > 266 = error "Invalid date"
  | n > 251 = (12, n - 251)
  | n > 220 = (11, n - 220)
  | n > 189 = (10, n - 189)
  | n > 158 = (9, n - 158)
  | n > 127 = (8, n - 127)
  | n > 96 = (7, n - 96)
  | n > 65 = (6, n - 65)
  | n > 34 = (5, n - 34)
  | n > 3 = (4, n - 3)
  | otherwise = (3, n)

showMonth :: Int -> String
showMonth m = case m of
    3 -> "MARCH"
    4 -> "APRIL" 
    5 -> "MAY"
    6 -> "JUNE"
    7 -> "JULY"
    8 -> "AUGUST"
    9 -> "SEPTEMBER"
    10 -> "OCTOBER"
    11 -> "NOVEMBER"
    12 -> "DECEMBER"
    _ -> error "Invalid date"

dateToStr :: (Int, Int) -> String
dateToStr (month, day) = showMonth month ++ " " ++ show day ++ " 1847"

-- Helper function to get weekday offset
weekdayOffset :: Int -> Int
weekdayOffset day = mod day 7
