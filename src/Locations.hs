module Locations where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Calendar qualified as Cal
import Data.Type.Nat
import Options
import State as S
import Text as T

-- | Types of locations in the game.
data LocationType = Town | Road deriving (Eq, Ord, Show)

-- | Specific locations that can be visited in the game.
data Location
  = Independence
  | MissouriRiver
  | FortKearney
  | FortLaramie
  | FortBridger
  | FortBoise
  | TheDalles
  | OregonCity
  | Roadside
  deriving (Eq, Ord)

-- | Get available commands based on the location type.
getOptions :: LocationType -> [Command]
getOptions Road = [Help, Status, Travel, Pace, Hunt, Quit]
getOptions Town = [Help, Status, Travel, Rest, Shop, Quit]

-- locationsToOptions :: String -> [Command]

-- | Custom string representation for each location.
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

-- | Determine the current location based on the mileage.
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

-- | Initial set of visited locations.
initialVisitedSet :: Set Location
initialVisitedSet = Set.fromList [Independence]

-- getLocation :: Nat -> Set Location -> (Location, Set Location)
-- getLocation n locs = if locationFromRange n `notElem` locs
--   then (locationFromRange n, Set.insert (locationFromRange n) locs)
--   else (Roadside, locs)

-- | Returns the next location and the updated location map
getLocation :: Nat -> Set Location -> Location
getLocation n locs =
  if locationFromRange n `notElem` locs
    then locationFromRange n
    else Roadside

-- >>> getLocation 0 Set.empty
-- (Independence, Missouri,fromList [Independence, Missouri])

-- DATES

-- | Display the date in a formatted string.
showDate :: Int -> String
showDate i = line ++ weekday ++ " " ++ day ++ "\n" ++ line
  where
    line = "==================================================\n"
    weekday = weekdays !! fromIntegral (weekdayOffset i)
    day = natToDate i

-- >>> showDate 266

natToCal :: Int -> Cal.Day
natToCal n = Cal.addDays (fromIntegral n) (Cal.fromGregorian 1847 1 1)

-- >>> natToCal 266

-- | Convert a numeric date to a formatted date string.
natToDate :: Int -> String
natToDate = show . natToCal

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
  | n == 0 = (3, 1)
  | otherwise = error "Invalid date"

-- | Convert a numeric month to its string representation.
showMonth :: Int -> String
showMonth m = case m of
  3 -> "March"
  4 -> "April"
  5 -> "May"
  6 -> "June"
  7 -> "July"
  8 -> "August"
  9 -> "September"
  10 -> "October"
  11 -> "November"
  12 -> "December"
  _ -> error "Invalid month"

-- | Convert a month and day to a formatted date string.
dateToStr :: (Int, Int) -> String
dateToStr (month, day) = showMonth month ++ " " ++ show day ++ " 1847"

-- Helper function to get weekday offset
weekdayOffset :: Int -> Int
weekdayOffset day = mod day 7
