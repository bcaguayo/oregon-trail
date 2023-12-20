module LocationsT where

import Locations
import Test.HUnit
    ( assertBool, assertEqual, runTestTT, Test(TestList, TestCase) )

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