module LocationsT where

import Locations
import Test.HUnit
import Data.Set (Set)
import Data.Set qualified as Set
import Options

testLocationFromRange :: Test
testLocationFromRange =
  TestList
    [ TestCase $ assertEqual "Independence" Independence (locationFromRange 0),
      TestCase $ assertEqual "Missouri River" MissouriRiver (locationFromRange 10),
      TestCase $ assertEqual "Fort Kearney" FortKearney (locationFromRange 325),
      TestCase $ assertEqual "Fort Laramie" FortLaramie (locationFromRange 600),
      TestCase $ assertEqual "Fort Bridger" FortBridger (locationFromRange 975),
      TestCase $ assertEqual "Fort Boise" FortBoise (locationFromRange 1500),
      TestCase $ assertEqual "The Dalles" TheDalles (locationFromRange 1880)
    ]

testNatToDate :: Test
testNatToDate = 
    TestList 
    [TestCase $ assertEqual "Date0" "1847-01-01" 
    (natToDate 0),
    TestCase $ assertEqual "Date266" "1847-09-24"
    (natToDate 266),
    TestCase $ assertEqual "Date50" "1847-02-20"
    (natToDate 50)]

tests :: Test
tests = TestList [testLocationFromRange, testNatToDate]