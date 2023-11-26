module Main where

-- import Lib
import Text
import GameState

main :: IO ()
main = username >> start

username :: IO()
username = do
    putStrLn "Enter your name: "
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

start :: IO()
start = do
    putStrLn Text.introShort
    input <- getLine
    case input of
        "1" -> putStrLn "Travel the trail" >> Main.options
        "2" -> putStrLn "Bye Bye!" >> return ()
        _ -> putStrLn "Invalid input, try again \n" >> start

options :: IO()
options = do
    putStrLn Text.options
    input <- getLine
    case input of
        "1" -> putStrLn "Travel the trail" >> Main.options
        "2" -> putStrLn "Bye Bye!" >> return ()
        _ -> putStrLn "Invalid input, try again \n" >> Main.options

-- 

{-
Basic Functionality:

1. Print intro
2. Print options
3. Get user input
4. Loop untill game is over
(mileage or date)
5. Print outro

-}

{-
WIP:
loop while game is not over

manage GameState

write tests

how do I make start + options generic on
line to print, options, next function
-}

--putStrLn Text.intro
