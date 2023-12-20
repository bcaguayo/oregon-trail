module Options where

import Data.Char (toLower)
import qualified Resources as R (ResourceType (..))

data Command
  = Help
  | Status
  | Pace
  | Travel
  | Shop
  | Rest
  | Hunt
  | Quit
  deriving (Eq, Show)

parseCommand :: String -> Maybe Command
parseCommand input = case map toLower input of
  "help" -> Just Help
  "quit" -> Just Quit
  "status" -> Just Status
  "travel" -> Just Travel
  "rest" -> Just Rest
  "shop" -> Just Shop
  "pace" -> Just Pace
  _ -> Nothing

-- This is a version that handles Int Commands
parseInt :: String -> Maybe Command
parseInt input = case map toLower input of
  "1" -> Just Travel
  "2" -> Just Status
  "3" -> Just Shop
  "4" -> Just Help
  "5" -> Just Rest
  "6" -> Just Pace
  "7" -> Just Quit
  _ -> Nothing

parseTownCommand :: String -> Maybe Command
parseTownCommand input = case map toLower input of
    "1" -> Just Travel
    "2" -> Just Status
    "3" -> Just Quit
    _ -> Nothing

data Profession = Banker | Carpenter | Farmer deriving (Eq, Show)

parseProfession :: String -> Maybe Profession
parseProfession input = case map toLower input of
  "banker" -> Just Banker
  "carpenter" -> Just Carpenter
  "farmer" -> Just Farmer
  _ -> Nothing

data ShopCommand
  = BuyFood
  | BuyClothes
  | BuyBullets
  | BuyOxen
  | BuyMedicine
  | BuyWheels
  | Leave
  deriving (Eq, Show)

parseShopCommand :: String -> Maybe R.ResourceType
parseShopCommand input = case input of
  "1" -> Just R.Food
  "2" -> Just R.Clothes
  "3" -> Just R.Bullets
  "4" -> Just R.Oxen
  "5" -> Just R.Medicine
  "6" -> Just R.Wheels
  _ -> Nothing