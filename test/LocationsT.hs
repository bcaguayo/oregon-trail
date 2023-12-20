module LocationsT where

import Locations
import Test.HUnit
import Data.Set (Set)
import Data.Set qualified as Set
import Options

-- Tests for the getOptions function.
testGetOptions :: Test
testGetOptions = TestList [
  "testGetOptionsRoad" ~: getOptions Road ~?= [Help, Status, Travel, Pace, Hunt, Quit],
  "testGetOptionsTown" ~: getOptions Town ~?= [Help, Status, Travel, Rest, Shop, Quit]
  ]

-- Tests for the locationFromRange function.
testLocationFromRange :: Test
testLocationFromRange = TestList [
  "testLocationFromRangeIndependence" ~: locationFromRange 0 ~?= Independence,
  "testLocationFromRangeMissouriRiver" ~: locationFromRange 20 ~?= MissouriRiver,
  "testLocationFromRangeFortKearney" ~: locationFromRange 400 ~?= FortKearney,
  "testLocationFromRangeFortLaramie" ~: locationFromRange 800 ~?= FortLaramie,
  "testLocationFromRangeFortBridger" ~: locationFromRange 1200 ~?= FortBridger,
  "testLocationFromRangeFortBoise" ~: locationFromRange 1700 ~?= FortBoise,
  "testLocationFromRangeTheDalles" ~: locationFromRange 1900 ~?= TheDalles
  ]

-- Tests for the getLocation function.
testGetLocation :: Test
testGetLocation = TestList [
  "testGetLocationNewLocation" ~: getLocation 20 (Set.fromList [Independence]) ~?= MissouriRiver,
  "testGetLocationExistingLocation" ~: getLocation 0 (Set.fromList [Independence]) ~?= Roadside
  ]

-- Tests for the natToDate function.
testNatToDate :: Test
testNatToDate = TestList [
  "testNatToDateEarly" ~: natToDate 5 ~?= "APRIL 2 1847",
  "testNatToDateMid" ~: natToDate 100 ~?= "JULY 4 1847",
  "testNatToDateLate" ~: natToDate 266 ~?= "DECEMBER 15 1847"
  ]

-- Tests for the showMonth function.
testShowMonth :: Test
testShowMonth = TestList [
  "testShowMonthMarch" ~: showMonth 3 ~?= "MARCH",
  "testShowMonthJune" ~: showMonth 6 ~?= "JUNE",
  "testShowMonthDecember" ~: showMonth 12 ~?= "DECEMBER"
  ]

-- Run all the tests.
tests :: Test
tests =
  TestList
    [ testGetOptions,
      testLocationFromRange,
      testGetLocation,
      testNatToDate,
      testShowMonth
    ]

runTest :: IO ()
runTest = runTestTT tests >>= print

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

testGetLocation :: Test
testGetLocation =
  TestList
    [ TestCase $ assertEqual "Independence" Independence (getLocation 0 initialVisitedSet),
      TestCase $ assertEqual "Missouri River" MissouriRiver (getLocation 10 initialVisitedSet),
      TestCase $ assertEqual "Fort Kearney" FortKearney (getLocation 325 initialVisitedSet),
      TestCase $ assertEqual "Fort Laramie" FortLaramie (getLocation 600 initialVisitedSet),
      TestCase $ assertEqual "Fort Bridger" FortBridger (getLocation 975 initialVisitedSet),
      TestCase $ assertEqual "Fort Boise" FortBoise (getLocation 1500 initialVisitedSet),
      TestCase $ assertEqual "The Dalles" TheDalles (getLocation 1880 initialVisitedSet)
    ]

testShowDate :: Test
testShowDate = 
    TestList 
    [TestCase $ assertEqual "Date0" "==================================================\nMonday 1847-01-01\n==================================================\n" 
    (showDate 0),
    TestCase $ assertEqual "Date266" "==================================================\nMonday 1847-09-24\n==================================================\n"
    (showDate 266),
    TestCase $ assertEqual "Date50" "==================================================\nMonday 1847-02-20\n==================================================\n"
    (showDate 50)]

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
tests = TestList [testLocationFromRange, testGetLocation, testShowDate, testNatToDate]