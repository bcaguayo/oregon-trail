module Main where

-- import Control.Monad
-- import Control.Monad.State
-- import GameState

-- import Control.Monad.RWS (MonadState(put))

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except
import GHC.Base (undefined)
import GameState
import Options
import State qualified as S
import StateMonad
import Text

play :: IO ()
play = username >> start >> options initialGameState

-- username :: (InputB m, Output m) => m ()
-- change putStrLn to output
-- change getLine to inputB

main :: IO ()
main = do
  -- let initial = S.execState (S.put initialGameState) undefined
  username >> start >> options initialGameState

-- S.evalState options' initialGameState
-- fst (S.runState options' initialGameState)

username :: IO ()
username = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")

profession :: IO ()
profession = undefined

marksmanship :: IO ()
marksmanship = undefined

start :: IO ()
start = do
  putStrLn Text.introShort

-- S.execState (S.put initialGameState) undefined
-- input <- getLine
-- case input of
--   "1" -> putStrLn "Travel the trail" >> Main.options
--   "2" -> quit
--   _ -> putStrLn "Invalid input, try again \n" >> start

-- options' :: GameStateM (IO ())
-- options' = do
--   gs <- S.get
--   return $ do
--     printGameState gs
--     putStrLn "playing ..."
--     input <- getLine
--     putStrLn input
--     let newGameState = S.execState (performActionM Shop) gs
--     printGameState newGameState

-- | This Game Loop is Cassia Approved
options :: GameState -> IO ()
options gs
  | mileage gs > 500 = putStrLn Text.endGood >> quit
  | date gs > 5 = putStrLn Text.endSlow >> quit
  | otherwise = do
      printGameState gs
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

-- Function to print the current game status
-- printGameStatus :: GS ()
-- printGameStatus = do
--   gameState <- S.get
--   -- You can use the gameState to print relevant information using printStatus
--   liftIO $ printStatus gameState

-- Assume this is the main loop of the game
-- gameLoop :: GameState -> IO ()
-- gameLoop gameState = do
--   putStrLn "Enter a command:"
--   input <- getLine
--   case parseCommand input of
--     Just command -> do
--       let ((), newGameState) = runState (performActionM command) gameState
--       gameLoop newGameState
--     Nothing -> putStrLn "Invalid command" >> gameLoop gameState

-- | tests using dummy IO data
-- test :: IO ()
-- test = do
--   putStrLn "Enter a command:"
--   input <- getLine
--   case parseCommand input of
--     Just command -> do
--       let (newGameState, ()) = runState (performActionM command) initialGameState
--       printGameState newGameState
--     Nothing -> putStrLn "Invalid command" >> test
printGameState :: GameState -> IO ()
printGameState gs = do
  putStrLn "___________________________\nGame State:"
  putStrLn ("Date: " ++ show (date gs))
  putStrLn ("Mileage: " ++ show (mileage gs))
  putStrLn ("Pace: " ++ show (pace gs))
  putStrLn ("Health: " ++ show (health gs))
  putStrLn ("Resources: " ++ show (resources gs))
  putStrLn "___________________________"

-- printGameState' = S $ \gs -> do
--   putStrLn "___________________________\nGame State:"
--   putStrLn ("Date: " ++ show (date gs))
--   putStrLn ("Mileage: " ++ show (mileage gs))
--   putStrLn ("Pace: " ++ show (pace gs))
--   putStrLn ("Health: " ++ show (health gs))
--   putStrLn ("Resources: " ++ show (resources gs))
--   putStrLn "___________________________"

-- printGameState'' :: GS ()
-- printGameState'' = do
--   gs <- S.get
--   printGameState gs

-- printGameStatus = S.get >>= printGameState

-- >>> printGameState initialGameState

-- WIP etc, check documentation

-- BEGIN: Generate I/O Tests
-- ioTests :: Test
-- ioTests = TestList
--     [ TestLabel "Test 1" test1
--     , TestLabel "Test 2" test2
--     -- Add more tests here
--     ]

-- test1 :: Test
-- test1 = TestCase $ do
--     -- Set up the initial state
--     let initialState = initialGameState

--     -- Perform the IO action
--     output <- someIOAction initialState

--     -- Check the expected output
--     assertEqual "Test 1 failed" expectedOutput output

-- test2 :: Test
-- test2 = TestCase $ do
--     -- Set up the initial state
--     let initialState = initialGameState

--     -- Perform the IO action
--     output <- someIOAction initialState

--     -- Check the expected output
--     assertEqual "Test 2 failed" expectedOutput output

-- END: Generate I/O Tests

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
IO dummy

how do I make start + options generic on
line to print, options, next function
-}

-- putStrLn Text.intro

{-

TicTacToe

data Player = X | O deriving (Eq, Show)
data Location = Loc Int Int deriving (Eq, Ord, Show)
type Board = M.Map Location Player
data Game = Game { board :: Board , current :: Player } deriving (Eq, Show)
data End = Win Player | Tie deriving (Eq, Show)

initialGame :: Game
checkEnd :: Board -> Maybe End
valid :: Board -> Location -> Bool
makeMove :: Game -> Location -> Maybe Game
showBoard :: Board -> String

class Monad m => Interface m where
   getMove :: Game -> m Location
   message :: String -> m ()
   playerMessage :: Player -> String -> m ()

locations :: [Location]
playGame :: Interface m => Game -> m ()

instance Interface IO where
   getMove :: Game -> IO Location
   playerMessage :: Player -> String -> IO ()
   message :: String -> IO ()

testCheckEnd :: Test
testCheckEnd = TestList [
    "Win for X"           ~: checkEnd winBoard ~?= Just (Win X)
  , "Initial is playable" ~: checkEnd (board initialGame) ~?= Nothing
  , "Tie game"            ~: checkEnd tieBoard ~?= Just Tie
  ] where
      winBoard = makeBoard [ [Just X,  Just X,  Just X ],
                             [Nothing, Just O,  Nothing],
                             [Nothing, Just O,  Nothing] ]
      tieBoard = makeBoard [ [Just X, Just O, Just X],
                             [Just O, Just X, Just O],
                             [Just O, Just X, Just X] ]
-- a quickcheck property about validity
prop_validMove :: Game -> Location -> Bool
prop_validMove game move =
  isJust (makeMove game move) == valid (board game) move
-- Arbitrary instances. These don't need to be complete yet,
-- but you should think about what types you will need to be able
-- to generate random values for.
instance Arbitrary Game where
  arbitrary = Game <$> arbitrary <*> arbitrary
instance Arbitrary Player where
  arbitrary = elements [X, O]
instance Arbitrary Location where
  arbitrary = elements locations

-}
