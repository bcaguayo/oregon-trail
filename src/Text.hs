module Text where

import System.IO ()

type Display = [String]

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

        "This version also takes reference from the Python 3.7 port\n" ++
        "made by philjonas on GitHub.\n\n" ++

        "PROGRAM NAME - 0REGON      VERSION:01/01/78\n" ++
        "ORIGINAL PROGRAMMING BY BILL HEINEMANN - 1971\n" ++
        "SUPPORT RESEARCH AND MATERIALS BY DON RAVITSCH\n" ++
        "MINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF\n" ++
        "CDC CYBER 70/73-26 BASIC 3-1\n" ++
        "DOCUMENTATION BOOKLET 'OREGON' AVAILABLE FROM\n" ++
        "  MECC SUPPORT SERVICES\n" ++
        "  2520 BROADWAY DRIVE\n" ++
        "  ST. PAUL, MN  55113\n"

introShort :: String
introShort = "Welcome to the Oregon Trail!\n"

option :: String
option =  "You may: \n" ++
          "1. Travel the trail \n" ++
          "2. Status \n" ++
          "3. Shop \n" ++
          "4. Help \n" ++
          "5. Rest \n" ++
          "6. Change the Pace \n" ++
          "7. Quit \n"

townOptions :: String
townOptions = "You may: \n" ++
              "1. Continue on trail \n" ++
              "2. Check supplies \n" ++
              "3. Quit \n"

help :: String
help = "To play: Type the number of your chosen command and press enter \n"

endGood :: String
endGood = "You have reached Oregon, Congratulations!\n"

endFood :: String
endFood = "You run out of food and your family starves to death\n"

endIll :: String
endIll = "You have died of dysentery\n"

endSlow :: String
endSlow = "You have been on the trail for too long. \n" ++
          "Your family dies in the first blizzard of winter \nGame Over\n"

instructionsQ :: String
instructionsQ =  "DO YOU NEED INSTRUCTIONS? (yes/no) \n"

instructionsA :: String
instructionsA = "THIS PROGRAM SIMULATES A TRIP OVER THE OREGON TRAIL FROM\n" ++
        "  INDEPENDENCE, MISSOURI TO OREGON CITY, OREGON IN 1847.\n" ++
        "  YOUR FAMILY OF FIVE WILL COVER THE 2040 MILE OREGON TRAIL\n" ++
        "  IN 5-6 MONTHS --- IF YOU MAKE IT ALIVE.\n\n" ++

        "  YOU HAD SAVED $900 TO SPEND FOR THE TRIP, AND YOU'VE JUST\n" ++
        "  PAID $200 FOR A WAGON.\n" ++
        "  YOU WILL NEED TO SPEND THE REST OF YOUR MONEY ON THE\n" ++
        "  FOLLOWING ITEMS:\n\n" ++

        -- "    OXEN - YOU CAN SPEND $200-$300 ON YOUR TEAM\n" ++
        -- "      THE MORE YOU SPEND, THE FASTER YOU'LL GO\n" ++
        -- "      BECAUSE YOU'LL HAVE BETTER ANIMALS\n\n" ++

        "    FOOD - THE MORE YOU HAVE, THE LESS CHANCE THERE\n" ++
        "      IS OF GETTING SICK\n\n" ++

        -- "    AMMUNITION - $1 BUYS A BELT OF 50 BULLETS\n" ++
        -- "      YOU WILL NEED BULLETS FOR ATTACKS BY ANIMALS\n" ++
        -- "      AND BANDITS, AND FOR HUNTING FOOD\n\n" ++

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

-- _______________________________ LOCATIONS _______________________________

dates :: [String]
dates = ["MARCH 29", "APRIL 12", "APRIL 26", "MAY 10", "MAY 24", "JUNE 7",
        "JUNE 21", "JULY 5", "JULY 19", "AUGUST 2", "AUGUST 16", "AUGUST 31", 
        "SEPTEMBER 13", "SEPTEMBER 27", "OCTOBER 11", "OCTOBER 25", "NOVEMBER 8", 
        "NOVEMBER 22", "DECEMBER 6", "DECEMBER 20"]

weekdays :: [String]
weekdays = ["MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY",
         "SATURDAY", "SUNDAY"]

professions :: String
professions = "What is your profession? \n" ++
             "1. Banker \n" ++
             "2. Carpenter \n" ++
             "3. Farmer \n" ++
             "4. What do these professions mean? \n"

pDescriptions :: String
pDescriptions = "1. Banker \n" ++
                "  You are a banker from Boston. \n" ++
                "  You have set aside $1200 for your trip to Oregon. \n" ++
                "  You are not good with a rifle \n" ++
                "2. Carpenter \n" ++
                "  You are a carpenter from Ohio. \n" ++
                "  You have set aside $900 for your trip to Oregon. \n" ++
                "  You are average with a rifle \n" ++
                "3. Farmer \n" ++
                "  You are a farmer from Illinois. \n" ++
                "  You have set aside $600 for your trip to Oregon. \n" ++
                "  You are proficient with a rifle \n"

printLocation :: String -> String -> String -> String
printLocation location date mileage = 
  "________{ " ++ location ++ " }________\n" ++
  "___________________________\n" ++
  "Date: " ++ date ++ "\n" ++ "Mileage: " ++ mileage ++ "\n" ++
  "___________________________\n"

printGameState :: String -> String -> String -> String -> String -> String
printGameState date mileage pace health resources = 
  "___________________________\nGame State:\n" ++
  "Date: " ++ date ++ "\n" ++ "Mileage: " ++ mileage ++ "\n" ++
  "Pace: " ++ pace ++ "\n" ++ "Health: " ++ health ++ "\n" ++
  "Resources: " ++ resources ++ "\n" ++
  "___________________________\n"

notEnough :: String
notEnough = "You don't have enough money for this purchase"

oxenRange :: String
oxenRange = "You can buy 2-4 oxen"

shopWelcome :: String
shopWelcome = "Welcome to the shop! \n" ++
              "You just spent $200 on a wagon. \n"

shopOptions :: String
shopOptions = "What would you like to buy? \n" ++
              "1. Food \n" ++
              "2. Clothes \n" ++
              "3. Bullets \n" ++
              "4. Oxen \n" ++
              "5. Medicine \n" ++
              "6. Wagon Wheels \n" ++
              "7. Leave \n"

illnessEvent :: String
illnessEvent = "You have fallen contracted "

illnesses :: [String]
illnesses = ["cholera", "dysentery", "measles", "typhoid", "a snakebite"]

eventTrade :: String
eventTrade = "You have encountered a group of friendly natives\n" ++
  "You can trade one set of clothes for 20 pounds of food\n"