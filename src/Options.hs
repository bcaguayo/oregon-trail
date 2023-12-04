
module Options where

import Data.Char (toLower)

data Command
    = Help
    | Quit
    | Status
    | Travel
    | Rest
    | Shop
    deriving (Eq, Show)

parseCommand :: String -> Maybe Command
parseCommand input = case map toLower input of
    "help" -> Just Help
    "quit" -> Just Quit
    "status" -> Just Status
    "travel" -> Just Travel
    "rest" -> Just Rest
    "shop" -> Just Shop
    _ -> Nothing

-- This is a version that handles Int Commands
parseInt :: String -> Maybe Command
parseInt input = case map toLower input of
    "1" -> Just Help
    "2" -> Just Quit
    "3" -> Just Status
    "4" -> Just Travel
    "5" -> Just Rest
    "6" -> Just Shop
    _ -> Nothing
