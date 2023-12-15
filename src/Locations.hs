module Locations where

import Data.Type.Nat
import Text as T
import Options
import qualified Data.Time.Calendar as Cal
import Data.Maybe (fromMaybe)

-- LOCATIONS

data LocationType = Town | River | Fort | Road deriving (Eq, Ord, Show)

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
getOptions Road = [Help, Status, Pace, Travel, Hunt, Quit]
getOptions Town = [Help, Status, Travel, Rest, Shop, Quit]
getOptions Fort = [Help, Status, Travel, Rest, Quit]
getOptions River = [Help, Status, Travel, Quit]

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

showDate :: Int -> String
showDate i = line ++ weekday ++ " " ++ day ++ line where
  line = "==================================================\n"
  weekday = weekdays !! fromIntegral (weekdayOffset i)
  day = natToDate i

-- >>> showDate 266
-- negate @Nat

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
