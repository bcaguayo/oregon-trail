module Main where

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except
-- | This module imports the 'State' module and qualifies it with the alias 'S'.
import qualified State as S
import qualified Text as T 
import Resources (ResourceType(..), shopTile, resCost, Resources (food))
import Data.Type.Nat
import GHC.Base (undefined)
import GameState
import Locations
import Options
import Trace
import Text.Read (readMaybe)

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
  -- Get User Name
  output "Enter your name: " >> inputb >>= (\name -> output ("Hello, " ++ name ++ "!"))
  -- Display Instructions
  output T.introShort
  output T.instructionsQ
  input <- inputb
  case input of
    "yes" -> output T.instructionsA
    "no" -> output "Good luck! \n"
    _ -> output "Invalid command, try again" >> play
  -- Choose your Profession
  track <- profession
  -- Choose what to buy
  output T.shopWelcome
  gs <- shopInitial track
  -- | Print GS
  output (printGameState gs)
  -- Play Game
  update gs


profession :: IO GameState
profession = do
  output T.professions
  job <- inputb
  case parseProfInt job of
    Just Professions -> output T.pDescriptions >> profession
    Just job -> case setTrack job of
      Just gs -> output ("You have chosen: " ++ show job) >> return gs
      Nothing -> output "Invalid profession, try again" >> profession
    Nothing -> output "Invalid profession, try again" >> profession

-- | This Game Loop is Cassia Approved
update :: GameState -> IO ()
update gs
  | food (resources gs) <= 0 = output T.endFood
  -- \| arrivedToLocation (mileage gs) = update' gs
  | mileage gs > 2040 = output T.endGood
  | date gs > 266 = output T.endSlow
  | otherwise = do
      output (printLocation gs)
      let (result, gs') = S.runState (runExceptT visitNewLocation) gs
      output T.option
      input <- inputb
      handleInput input gs'

-- | We're not using this so far
update' :: GameState -> IO ()
update' gs = do
  output ("You are in: " ++ show (locationFromRange (mileage gs)))
  output T.townOptions
  input <- inputb
  case parseTownCommand input of
    Just Travel -> do
      let newGameState = S.runState (runExceptT (performActionM Travel)) gs
      case newGameState of
        (Left errMsg, _) -> output errMsg >> update gs
        (Right _, updatedGameState) -> output "Traveling... \n" >> update updatedGameState
    Just Status -> output (printGameState gs) >> update' gs
    Just Quit -> output "Bye Bye!"
    _ -> output "Invalid command, try again \n" >> update' gs

handleInput :: String -> GameState -> IO ()
handleInput input gs =
  case parseInt input of
    Just Travel -> do
      let newGameState = S.runState (runExceptT (performActionM Travel)) gs
      case newGameState of
        (Left errMsg, _) -> output errMsg >> update gs
        (Right _, updatedGameState) ->
          output "Traveling... \n" >> update (nextDate updatedGameState)
    Just Status -> output (printGameState gs) >> update gs
    Just Shop -> shopOption gs >>= update
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
    Just Quit -> output "Bye Bye!"
    Nothing -> output "Invalid command, try again \n" >> update gs

-- | For each resource we need, shop tile and cost per unit
shopping :: GameState -> ResourceType -> IO GameState
shopping gs rt = do
  -- | get components according to resource type
  let resName = shopTile rt
      resPrice = resCost rt
  -- | ask user how many of the resource they want to buy
  output ("How many " ++ resName ++ " do you want to buy?")
  amount <- inputb  
  let natAmount = case readMaybe amount of
        Nothing -> 0
        Just amount -> if amount < 0 then 0 
          else fromIntegral amount :: Nat
  -- | if they don't have enough money, throw error
  if natAmount * 10 > getMoney gs
    then output T.notEnough >> shopping gs rt
    -- | otherwise, update game state
    else if rt == Oxen && (natAmount < 2 || natAmount > 4)
    then output T.oxenRange >> shopping gs rt
    else if natAmount == 0
    then return gs
    else do
      let newGameState = shopAction gs rt natAmount
      output ("You have bought " ++ show natAmount ++ " " ++ resName ++  " \n") >> return newGameState

-- | At the beggining of the game, call all functions sequentially
shopInitial :: GameState -> IO GameState
shopInitial gs = do
  gsOne <- shopping gs Oxen
  gsTwo <- shopping gsOne Food
  gsThree <- shopping gsTwo Clothes
  gsFour <- shopping gsThree Bullets
  gsFive <- shopping gsFour Medicine
  shopping gsFive Wheels

-- | If choosing the Shop option, call shop individually
shopOption :: GameState -> IO GameState
shopOption gs = do
  output T.shopOptions
  input <- inputb
  case parseShopCommand input of
    Just rt -> shopping gs rt
    Nothing -> output "Leaving Shop" >> return gs
