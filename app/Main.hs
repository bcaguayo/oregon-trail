module Main where

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except
-- | This module imports the 'State' module and qualifies it with the alias 'S'.
import qualified State as S
import StateMonad
import qualified Text as T
import Resources (ResourceType(..), shopTile, resCost)
import Data.Type.Nat
import GHC.Base (undefined)
import GameState
import Locations
import Options
import Trace

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
  output "Enter your name: " >> inputb >>= output . ("Hello, " ++) >> output "!"
  -- Display Instructions
  output T.introShort
  output T.instructionsQ
  input <- inputb
  case input of
    "yes" -> output T.instructionsA
    "no" -> output "Good luck! \n"
    _ -> output "Invalid command, try again" >> play
  -- Choose your Profession
  output T.professions >> inputb >>= output . ("You have chosen: " ++)
  -- Play Game
  update initialGameState

-- | This Game Loop is Cassia Approved
update :: GameState -> IO ()
update gs
  -- \| arrivedToLocation (mileage gs) = update' gs
  | mileage gs > 2040 = output T.endGood
  | date gs > 266 = output T.endSlow
  | otherwise = do
      output (printLocation gs)
      output T.option
      input <- inputb
      handleInput input gs

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
    Just Quit -> output "Bye Bye!"
    Nothing -> output "Invalid command, try again \n" >> update gs

printLocation gs = T.printLocation l d m
  where
    l = show (locationFromRange (mileage gs))
    d = show (date gs)
    m = show (mileage gs)

printGameState gs = T.printGameState d m p h r
  where
    d = show (date gs)
    m = show (mileage gs)
    p = show (pace gs)
    h = show (health gs)
    r = show (resources gs)

-- shopping :: GameState -> IO ()
-- shopping gs = do
--   output "How many pounds of food do you want to buy?"
--   food <- inputb
--   if food * 10 > getMoney gs
--     then output "You don't have enough money to buy that much food"
--     else do
--       output "How many sets of clothes do you want to buy?"
--       clothes <- inputb
--       if clothes * 10 > money (resources gs) - food * 10
--         then output "You don't have enough money to buy that much clothes"
--         else do
--           let newResources = addResources (resources gs) Food (food * 10)
--           let newResources' = addResources newResources Clothes (clothes * 10)
--           let newGameState = gs {resources = newResources'}
--           output (printGameState newGameState)
--           update newGameState
--   -- if food less than cash, throw error
--   output "How many sets of clothes do you want to buy?"
--   clothes <- inputb
--   -- if clothes less than cash - food, throw error
--   -- newGameState <- S.runState (runExceptT (performActionM (Shop food clothes))) gs
--   output (printGameState initialGameState)

-- | For each resource we need, shop tile and cost per unit

shopping gs rt = do
  -- | get components according to resource type
  let resName = shopTile rt
      resPrice = resCost rt
  -- | ask user how many of the resource they want to buy
  output ("How many " ++ resName ++ " do you want to buy?")
  amount <- inputb
  let natAmount = fromIntegral (read amount :: Int) :: Nat
  -- | if they don't have enough money, throw error
  if natAmount * 10 > getMoney gs
    then output T.notEnough >> return gs
    -- | otherwise, update game state
    else do
      let newGameState = shopAction gs Food natAmount
      output ("You have bought " ++ show natAmount ++ " " ++ resName ++  " \n") >> return newGameState

-- | At the beggining of the game, call all functions sequentially
shopInitial gs = do
  gsOne <- shopping gs Food
  gsTwo <- shopping gsOne Clothes
  gsThree <- shopping gsTwo Bullets
  gsFour <- shopping gsThree Oxen
  gsFive <- shopping gsFour Medicine
  shopping gsFive Wheels

-- | If choosing the Shop option, call shop individually
shopOption gs = do
  output T.shopOptions
  input <- inputb
  case parseShopCommand input of
    Just rt -> shopping gs rt
    Nothing -> output "Invalid command, try again" >> shopOption gs