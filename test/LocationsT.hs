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