module Options where

import Data.Char (toLower)
import Resources qualified as R (ResourceType (..))

-- | Enumerated type for game commands.
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

-- | Parses a string input into a Command based on integer input.
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

-- | Special parser for commands available in a town location.
parseTownCommand :: String -> Maybe Command
parseTownCommand input = case map toLower input of
  "1" -> Just Travel
  "2" -> Just Status
  "3" -> Just Quit
  _ -> Nothing

data Profession = Banker | Carpenter | Farmer | Professions deriving (Eq, Show)

parseProfInt :: String -> Maybe Profession
parseProfInt input = case map toLower input of
  "1" -> Just Banker
  "2" -> Just Carpenter
  "3" -> Just Farmer
  "4" -> Just Professions
  _ -> Nothing

-- | Enumerated type for commands available in the shop.
data ShopCommand
  = BuyFood
  | BuyClothes
  | BuyBullets
  | BuyOxen
  | BuyMedicine
  | BuyWheels
  | Leave
  deriving (Eq, Show)

-- | Parse a string input into a ResourceType for shopping, if possible.
parseShopCommand :: String -> Maybe R.ResourceType
parseShopCommand input = case input of
  "1" -> Just R.Food
  "2" -> Just R.Clothes
  "3" -> Just R.Bullets
  "4" -> Just R.Oxen
  "5" -> Just R.Medicine
  "6" -> Just R.Wheels
  _ -> Nothing  