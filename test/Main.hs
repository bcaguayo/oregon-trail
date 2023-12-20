module Main where

import Test.HUnit
import Test.QuickCheck

import GameStateT ( tests )
import EventsT ( tests )
import ResourcesT ( runTests )

main :: IO()
main = do
    putStrLn "gamestate testing ..."
    runTestTT GameStateT.tests >>= print
    putStrLn "resource testing ..."
    ResourcesT.runTests
    putStrLn "event testing ..."
    runTestTT EventsT.tests >>= print