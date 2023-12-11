module Main where

import GameState
    ( GameState(resources, date, mileage, pace, health),
      Pace(Fast, Slow),
      initialGameState,
      performActionM )
import Options
    ( Command(Quit, Travel, Status, Shop, Help, Rest, Pace), parseInt )
import GHC.Base (undefined)
-- import Control.Monad.RWS (MonadState(put))

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except
import GHC.Base (undefined)
import GameState
import Options
import State qualified as S
import StateMonad
import Control.Monad.Cont (MonadIO(liftIO))
import Text

{-
Basic Functionality:

1. Print intro
2. Print options
3. Get user input
4. Loop until game is over
(mileage or date)
5. Print outro
-}

main :: IO ()
main = play

play :: IO ()
play = do
  putStrLn Text.version
  username
  putStrLn Text.introShort
  instructions
  options initialGameState

username :: IO ()
username = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")

profession :: IO ()
profession = undefined

marksmanship :: IO ()
marksmanship = undefined

instructions :: IO ()
instructions = do
  putStrLn Text.instructions1
  input <- getLine
  case input of
    "yes" -> putStrLn Text.instructions2
    "no" -> putStrLn "Good luck! \n"
    _ -> putStrLn "Invalid command, try again" >> instructions

-- | This Game Loop is Cassia Approved
options :: GameState -> IO ()
options gs
  | mileage gs > 500 = putStrLn Text.endGood >> quit
  | date gs > 5 = putStrLn Text.endSlow >> quit
  | otherwise = do
      printLocation gs
      putStrLn Text.option
      input <- getLine
      case parseInt input of
        Just Travel -> do
          let newGameState = S.runState (runExceptT (performActionM Travel)) gs
          case newGameState of
            (Left errMsg, _) -> putStrLn errMsg >> options gs
            (Right _, updatedGameState) -> putStrLn "Traveling... \n" >> options updatedGameState
        Just Status -> printGameState gs >> options gs
        Just Shop -> do
          let newGameState = S.runState (runExceptT (performActionM Shop)) gs
          case newGameState of
            (Left errMsg, _) -> putStrLn errMsg >> options gs
            (Right _, updatedGameState) -> putStrLn "You have bought some stuff \n" >> options updatedGameState
        Just Help -> putStrLn Text.help >> options gs
        Just Rest -> do
          let newGameState = S.runState (runExceptT (performActionM Rest)) gs
          case newGameState of
            (Left errMsg, _) -> putStrLn errMsg >> options gs
            (Right _, updatedGameState) -> putStrLn "Resting... \n" >> options updatedGameState
        Just Pace -> do
          let newGameState = S.runState (runExceptT (performActionM Pace)) gs
          case newGameState of
            (Left errMsg, _) -> putStrLn errMsg >> options gs
            (Right _, updatedGameState) ->
              putStrLn (if pace updatedGameState == Slow then "Going Slow ...\n" else "Going Fast ...\n") >> options updatedGameState
        Just Quit -> quit
        Nothing -> putStrLn "Invalid command, try again \n" >> options gs

quit :: IO ()
quit = putStrLn "Bye Bye!"

printLocation :: GameState -> IO ()
printLocation gs = do
  putStrLn "___________________________\n"
  -- putStrLn ("Location: " ++ "not implemented")
  putStrLn ("Date: " ++ show (date gs))
  putStrLn ("Mileage: " ++ show (mileage gs))
  putStrLn "___________________________"

printGameState :: GameState -> IO ()
printGameState gs = do
  putStrLn "___________________________\nGame State:"
  putStrLn ("Date: " ++ show (date gs))
  putStrLn ("Mileage: " ++ show (mileage gs))
  putStrLn ("Pace: " ++ show (pace gs))
  putStrLn ("Health: " ++ show (health gs))
  putStrLn ("Resources: " ++ show (resources gs))
  putStrLn "___________________________"
