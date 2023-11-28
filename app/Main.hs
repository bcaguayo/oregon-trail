module Main where

import Control.Monad (void)
import GameState
import Text

main :: IO ()
main = username >> start

username :: IO ()
username = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")

start :: IO ()
start = do
  putStrLn Text.introShort
  input <- getLine
  case input of
    "1" -> putStrLn "Travel the trail" >> Main.options
    "2" -> void (putStrLn "Bye Bye!")
    _ -> putStrLn "Invalid input, try again \n" >> start

options :: IO ()
options = do
  putStrLn Text.options
  input <- getLine
  case input of
    "1" -> putStrLn "Travel the trail" >> Main.options
    "2" -> void (putStrLn "Bye Bye!")
    _ -> putStrLn "Invalid input, try again \n" >> Main.options

-- Assume this is the main loop of the game
gameLoop :: GameState -> IO ()
gameLoop gameState = do
  putStrLn "Enter a command:"
  input <- getLine
  case parseCommand input of
    Just command -> do
      let ((), newGameState) = runState (performActionM command) gameState
      gameLoop newGameState
    Nothing -> putStrLn "Invalid command" >> gameLoop gameState

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

-- putStrLn Text.intro
