module Text where

-- import Control.Monad.State
-- import qualified Mtl as S
import System.IO ()

--
-- putStrLn to print to terminal
--  use StateMOnad

type Display = [String]

-- | Store a string in the state
-- storeString :: String -> State Display ()
-- storeString s = modify (s:)
-- storeString :: String -> IO ()
-- storeString s = putStrLn s

-- | Print all stored strings to the terminal
-- printString :: State Display ()
-- printString = do
--   strings <- get
--   liftIO $ mapM_ putStrLn strings

-- | Fetch the most recently stored string
-- fetchString :: State Display (Maybe String)
-- fetchString = do
--   strings <- get
--   case strings of
--     [] -> return Nothing
--     (s:ss) -> do
--       put ss
--       return (Just s)

fetchString :: IO ()
fetchString = putStrLn "Choose your profession:"

-- | Example usage
version :: String
version = "This Haskell implementation presents a rendition of\n" ++
        "the 1978 version of the Oregon Trail game originally\n" ++
        "developed by Don Rawitsch, Bill Heinemann, and Paul Dillenberger\n" ++
        "for the HP time-shared BASIC platform in 1971.\n" ++
        "Based on the version published in the July-August 1978\n" ++
        "issue of Creative Computing magazine.\n\n" ++

        "PROGRAM NAME - 0REGON      VERSION:01/01/78\n" ++
        "ORIGINAL PROGRAMMING BY BILL HEINEMANN - 1971\n" ++
        "SUPPORT RESEARCH AND MATERIALS BY DON RAVITSCH\n" ++
        "MINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF\n" ++
        "CDC CYBER 70/73-26 BASIC 3-1\n" ++
        "DOCUMENTATION BOOKLET 'OREGON' AVAILABLE FROM\n" ++
        "  MECC SUPPORT SERVICES\n" ++
        "  2520 BROADWAY DRIVE\n" ++
        "  ST. PAUL, MN  55113\n"


-- PROGRAM NAME - 0REGON        VERSION:01/01/78"      ++ "\n" 
--         ++ "ORIGINAL PROGRAMMING BY BILL HEINEMANN - 1971"      ++ "\n"
--         ++ "SUPPORT RESEARCH AND MATERIALS BY DON RAVITSCH"     ++ "\n"
--         ++ "MINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF"   ++ "\n"
--         ++ "CDC CYBER 70/73-26 BASIC 3-1"

-- let version = "PROGRAM NAME - OREGON    VERSION:01/01/78" ++ "\n" ++
--               "ORIGINAL PROGRAMMING BY BILL HEINEMANN - 1971" ++ "\n" ++
--               "SUPPORT RESEARCH AND MATERIALS BY DON RAVITSCH" ++ "\n" ++
--               "MINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF" ++ "\n" ++
--               "CDC CYBER 70/73-26 BASIC 3-1"

intro :: String
intro = "THIS PROGRAM SIMULATES A TRIP OVER THE OREGON TRAIL FROM\n"
        ++ "INDEPENDENCE, MISSOURI TO OREGON CITY, OREGON IN 1847.\n"
        ++ "YOUR FAMILY OF FIVE WILL COVER THE 2040 MILE OREGON TRAIL\n"
        ++ "IN 5-6 MONTHS --- IF YOU MAKE IT ALIVE.\n\n"
        ++ "YOU HAD SAVED $900 TO SPEND FOR THE TRIP, AND YOU'VE JUST\n"
        ++ "  PAID $200 FOR A WAGON .\n"
        ++ "YOU WILL NEED TO SPEND THE REST OF YOUR MONEY ON THE\n"
        ++ "  FOLLOWING ITEMS:\n\n"
        ++ "   OXEN - YOU CAN SPEND $200-$300 ON YOUR TEAM\n"
        ++ "      THE MORE YOU SPEND, THE FASTER YOU'LL GO\n"
        ++ "        BECAUSE YOU'LL HAVE BETTER ANIMALS\n\n"
        ++ "   FOOD - THE MORE YOU HAVE, THE LESS CHANCE THERE\n"
        ++ "        IS OF GETTING SICK\n\n"
        ++ "   AMMUNITION - $1 BUYS A BELT OF 50 BULLETS\n"
        ++ "      YOU WILL NEED BULLETS FOR ATTACKS BY ANIMALS\n"
        ++ "        AND BANDITS, AND FOR HUNTING FOOD\n\n"
        ++ "   CLOTHING - THIS IS ESPECIALLY IMPORTANT FOR THE COLD\n"
        ++ "        WEATHER YOU WILL ENCOUNTER WHEN CROSSING\n"
        ++ "        THE MOUNTAINS\n\n"
        ++ "   MISCELLANEOUS SUPPLIES - THIS INCLUDES MEDICINE AND\n"
        ++ "        OTHER THINGS YOU WILL NEED FOR SICKNESS\n"
        ++ "        AND EMERGENCY REPAIRS\n\n"
        ++ "YOU CAN SPEND ALL YOUR MONEY BEFORE YOU START YOUR TRIP -\n"
        ++ "OR YOU CAN SAVE SOME OF YOUR CASH TO SPEND AT FORTS ALONG\n"
        ++ "THE WAY WHEN YOU RUN LOW. H0WEVER, ITEMS COST MORE AT\n"
        ++ "THE FORTS. YOU CAN ALSO GO HUNTING ALONG THE WAY TO GET\n"
        ++ "MORE FOOD.\n"
        ++ "WHENEVER YOU HAVE TO USE YOUR TRUSTY RIFLE ALONG THE WAY,\n"
        ++ "YOU WILL BE TOLD TO TYPE IN A WORD (ONE THAT SOUNDS LIKE A\n"
        ++ "GUN SHOT). THE FASTER YOU TYPE IN THAT WORD AND HIT THE\n"
        ++ "\"RETURN\" KEY, THE BETTER LUCK YOU'LL HAVE WITH YOUR GUN."
        ++ "\nAT EACH TURN, ALL ITEMS ARE SHOWN IN DOLLAR AMOUNTS\n"
        ++ "EXCEPT BULLETS\n"
        ++ "WHEN ASKED TO ENTER MONEY AMOUNTS, DON'T USE A \"$\".\n\n"
        ++ "GOOD LUCK!!!"

-- main :: IO ()
-- main = do
--   putStrLn version

-- >>> text1
-- "PROGRAM NAME - 0REGON        VERSION:01/01/78\nORIGINAL PROGRAMMING BY BILL HEINEMANN - 1971\nSUPPORT RESEARCH AND MATERIALS BY DON RAVITSCH\nMINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF\nCDC CYBER 70/73-26 BASIC 3-1"

-- module Text (textList) where

-- textList :: [String]
-- textList = ["Welcome to Oregon Trail!", "Choose your profession:", "1. Banker", "2. Carpenter", "3. Farmer"]

introShort :: String
introShort = "Welcome to the Oregon Trail!\n"
        --      "You may: \n" ++
        --      "1. Travel the trail \n" ++
        --      "2. Quit \n"

option =  "You may: \n" ++
          "1. Travel the trail \n" ++
          "2. Status \n" ++
          "3. Shop \n" ++
          "4. Help \n" ++
          "5. Rest \n" ++
          "6. Change the Pace \n" ++
          "7. Quit \n"

help = "To play: Type the number of your chosen command and press enter \n"

endGood :: String
endGood = "You have reached Oregon, Congratulations!\n"

endIll :: String
endIll = "You have died of dysentery\n"

endSlow :: String
endSlow = "You have been on the trail for too long. \n" ++
          "Your family dies in the first blizzard of winter \nGame Over\n"

dates :: [String]
dates = ["March 29", "April 12", "April 26", "May 10", "May 24", "June 7",
        "June 21", "July 5", "July 19", "August 2", "August 16", "August 30", 
        "September 13", "September 27", "October 11", "October 25", "November 8", 
        "November 22", "December 6", "December 20"]

instructions :: String
instructions = "THIS PROGRAM SIMULATES A TRIP OVER THE OREGON TRAIL FROM\n" ++
        "  INDEPENDENCE, MISSOURI TO OREGON CITY, OREGON IN 1847.\n" ++
        "  YOUR FAMILY OF FIVE WILL COVER THE 2040 MILE OREGON TRAIL\n" ++
        "  IN 5-6 MONTHS --- IF YOU MAKE IT ALIVE.\n\n" ++

        "  YOU HAD SAVED $900 TO SPEND FOR THE TRIP, AND YOU'VE JUST\n" ++
        "  PAID $200 FOR A WAGON.\n" ++
        "  YOU WILL NEED TO SPEND THE REST OF YOUR MONEY ON THE\n" ++
        "  FOLLOWING ITEMS:\n\n" ++

        "    OXEN - YOU CAN SPEND $200-$300 ON YOUR TEAM\n" ++
        "      THE MORE YOU SPEND, THE FASTER YOU'LL GO\n" ++
        "      BECAUSE YOU'LL HAVE BETTER ANIMALS\n\n" ++

        "    FOOD - THE MORE YOU HAVE, THE LESS CHANCE THERE\n" ++
        "      IS OF GETTING SICK\n\n" ++

        "    AMMUNITION - $1 BUYS A BELT OF 50 BULLETS\n" ++
        "      YOU WILL NEED BULLETS FOR ATTACKS BY ANIMALS\n" ++
        "      AND BANDITS, AND FOR HUNTING FOOD\n\n" ++

        "    CLOTHING - THIS IS ESPECIALLY IMPORTANT FOR THE COLD\n" ++
        "      WEATHER YOU WILL ENCOUNTER WHEN CROSSING THE MOUNTAINS\n\n" ++

        "    MISCELLANEOUS SUPPLIES - THIS INCLUDES MEDICINE AND\n" ++
        "      OTHER THINGS YOU WILL NEED FOR SICKNESS\n" ++
        "      AND EMERGENCY REPAIRS\n\n" ++

        "  YOU CAN SPEND ALL YOUR MONEY BEFORE YOU START YOUR TRIP -\n" ++
        "  OR YOU CAN SAVE SOME OF YOUR CASH TO SPEND AT FORTS ALONG\n" ++
        "  THE WAY WHEN YOU RUN LOW. HOWEVER, ITEMS COST MORE AT\n" ++
        "  THE FORTS. YOU CAN ALSO GO HUNTING ALONG THE WAY TO GET\n" ++
        "  MORE FOOD.\n\n" ++

        "  WHENEVER YOU HAVE TO USE YOUR TRUSTY RIFLE ALONG THE WAY,\n" ++
        "  YOU WILL BE TOLD TO TYPE IN A WORD (ONE THAT SOUNDS LIKE A\n" ++
        "  GUN SHOT). THE FASTER YOU TYPE IN THAT WORD AND HIT THE\n" ++
        "  \"RETURN\" KEY, THE BETTER LUCK YOU'LL HAVE WITH YOUR GUN.\n\n" ++

        "  AT EACH TURN, ALL ITEMS ARE SHOWN IN DOLLAR AMOUNTS\n" ++
        "  EXCEPT BULLETS.\n" ++
        "  WHEN ASKED TO ENTER MONEY AMOUNTS, DON'T USE A \"$\".\n" ++
        "  GOOD LUCK!!!"
