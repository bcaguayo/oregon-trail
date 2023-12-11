module Main where

import GameState
    ( GameState(resources, date, mileage, pace, health),
      Pace(Fast, Slow),
      initialGameState,
      performActionM, nextDate )
import Options
import GHC.Base (undefined)
-- import Control.Monad.RWS (MonadState(put))

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except
import GHC.Base (undefined)
import GameState
-- | This module imports the 'State' module and qualifies it with the alias 'S'.
import qualified State as S
import StateMonad
import Control.Monad.Cont (MonadIO(liftIO))
import qualified Text as T
import Trace
import Locations (getLocation)

{-
Basic Functionality:

1. Print intro
2. Print update
3. Get user input
4. Loop until game is over
(mileage or date)
5. Print outro
-}

main :: IO ()
main = play

-- for IO, isolate input output
play :: IO ()
play = do
  output T.version
  username -- output "Enter your name: " >>= inputb >>= output . ("Hello, " ++) >> output "!"
  output T.introShort
  instructions
  update initialGameState

username :: IO ()
username = do
  output "Enter your name: "
  name <- inputb
  output ("Hello, " ++ name ++ "!")

profession :: IO ()
profession = undefined

marksmanship :: IO ()
marksmanship = undefined

instructions :: IO ()
instructions = do
  output T.instructions1
  input <- inputb
  case input of
    "yes" -> output T.instructions2
    "no" -> output "Good luck! \n"
    _ -> output "Invalid command, try again" >> instructions

-- | This Game Loop is Cassia Approved
update :: GameState -> IO ()
update gs
  | mileage gs > 2040 = output T.endGood >> quit
  | date gs > 266 = output T.endSlow >> quit
  | otherwise = do
      printLocation gs
      output T.option
      input <- inputb
      handleInput input gs

quit :: IO ()
quit = output "Bye Bye!"

handleInput :: String -> GameState -> IO ()
handleInput input gs =
  case parseInt input of
    Just Travel -> do
      let newGameState = S.runState (runExceptT (performActionM Travel)) gs
      case newGameState of
        (Left errMsg, _) -> output errMsg >> update gs
        (Right _, updatedGameState) ->
          output "Traveling... \n" >> update (nextDate updatedGameState)
    Just Status -> printGameState gs >> update gs
    Just Shop -> do
      let newGameState = S.runState (runExceptT (performActionM Shop)) gs
      case newGameState of
        (Left errMsg, _) -> output errMsg >> update gs
        (Right _, updatedGameState) ->
          output "You have bought some stuff \n" >> update updatedGameState
    Just Help -> output T.help >> update gs
    Just Hunt -> do
      let newGameState = S.runState (runExceptT (performActionM Hunt)) gs
      case newGameState of
        (Left errMsg, _) -> output errMsg >> update gs
        (Right _, updatedGameState) ->
          output "Hunting... \n" >> update (nextDate updatedGameState)
    Just Rest -> do
      let newGameState = S.runState (runExceptT (performActionM Rest)) gs
      case newGameState of
        (Left errMsg, _) -> output errMsg >> update gs
        (Right _, updatedGameState) ->
          output "Resting... \n" >> update (nextDate updatedGameState)
    Just Pace -> do
      let newGameState = S.runState (runExceptT (performActionM Pace)) gs
      case newGameState of
        (Left errMsg, _) -> output errMsg >> update gs
        (Right _, updatedGameState) ->
          output (if pace updatedGameState == Slow then "Going Slow ...\n" else "Going Fast ...\n") >> update updatedGameState
    Just Quit -> quit
    Nothing -> output "Invalid command, try again \n" >> update gs

-- define as string

printLocation :: GameState -> IO ()
printLocation gs = do
  output ("________{ " ++ getLocation (mileage gs) ++ " }________\n")
  output "___________________________\n"
  -- output ("Location: " ++ "not implemented")
  output ("Date: " ++ show (date gs))
  output ("Mileage: " ++ show (mileage gs))
  output "___________________________"

printGameState :: GameState -> IO ()
printGameState gs = do
  output "___________________________\nGame State:"
  output ("Date: " ++ show (date gs))
  output ("Mileage: " ++ show (mileage gs))
  output ("Pace: " ++ show (pace gs))
  output ("Health: " ++ show (health gs))
  output ("Resources: " ++ show (resources gs))
  output "___________________________"

shopping :: GameState -> IO ()
shopping gs = undefined


-- doEvent :: IO ()
-- doEvent = do
--   gen <- newStdGen
--   if random gen then output "You found a river" else output "You found a town"