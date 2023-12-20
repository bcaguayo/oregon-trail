module Main where

import Test.HUnit
import Test.QuickCheck

import GameStateT ( tests )
import EventsT ( tests )
import ResourcesT ( runTests )
import OptionsT ( tests )
import LocationsT ( tests )

main :: IO()
main = do
    putStrLn "gamestate testing ..."
    runTestTT GameStateT.tests >>= print
    putStrLn "resource testing ..."
    ResourcesT.runTests
    putStrLn "event testing ..."
    runTestTT EventsT.tests >>= print
    putStrLn "options testing ..."
    runTestTT OptionsT.tests >>= print
    putStrLn "locations testing ..."
    runTestTT LocationsT.tests >>= print