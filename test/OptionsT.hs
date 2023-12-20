module OptionsT where

import Test.HUnit
import Options
import Resources qualified as R (ResourceType (..))

-- Tests for the parseCommand function.testParseCommand :: Test
testParseCommand = TestList [
  "testParseCommandHelp" ~: parseCommand "help" ~?= Just Help,
  "testParseCommandQuit" ~: parseCommand "quit" ~?= Just Quit,
  "testParseCommandStatus" ~: parseCommand "status" ~?= Just Status,
  "testParseCommandTravel" ~: parseCommand "travel" ~?= Just Travel,
  "testParseCommandRest" ~: parseCommand "rest" ~?= Just Rest,
  "testParseCommandInvalid" ~: parseCommand "invalid" ~?= Nothing
  ]

-- Tests for the parseInt function.
testParseInt :: Test
testParseInt = TestList [
  "testParseIntTravel" ~: parseInt "1" ~?= Just Travel,
  "testParseIntStatus" ~: parseInt "2" ~?= Just Status,
  "testParseIntShop" ~: parseInt "3" ~?= Just Shop,
  "testParseIntHelp" ~: parseInt "4" ~?= Just Help,
  "testParseIntRest" ~: parseInt "5" ~?= Just Rest,
  "testParseIntInvalid" ~: parseInt "invalid" ~?= Nothing
  ]

-- Tests for the parseTownCommand function.
testParseTownCommand :: Test
testParseTownCommand = TestList [
  "testParseTownCommandTravel" ~: parseTownCommand "1" ~?= Just Travel,
  "testParseTownCommandStatus" ~: parseTownCommand "2" ~?= Just Status,
  "testParseTownCommandQuit" ~: parseTownCommand "3" ~?= Just Quit,
  "testParseTownCommandInvalid1" ~: parseTownCommand "invalid" ~?= Nothing,
  "testParseTownCommandInvalid2" ~: parseTownCommand "4" ~?= Nothing,
  "testParseTownCommandInvalid3" ~: parseTownCommand "0" ~?= Nothing
  ]

-- Tests for the parseProfession function.
testParseProfession :: Test
testParseProfession = TestList [
  "testParseProfessionBanker" ~: parseProfession "banker" ~?= Just Banker,
  "testParseProfessionCarpenter" ~: parseProfession "carpenter" ~?= Just Carpenter,
  "testParseProfessionFarmer" ~: parseProfession "farmer" ~?= Just Farmer,
  "testParseProfessionInvalid1" ~: parseProfession "invalid" ~?= Nothing,
  "testParseProfessionInvalid2" ~: parseProfession "teacher" ~?= Nothing,
  "testParseProfessionInvalid3" ~: parseProfession "doctor" ~?= Nothing
  ]

-- Tests for the parseShopCommand function.
testParseShopCommand :: Test
testParseShopCommand = TestList [
  "testParseShopCommandFood" ~: parseShopCommand "1" ~?= Just R.Food,
  "testParseShopCommandClothes" ~: parseShopCommand "2" ~?= Just R.Clothes,
  "testParseShopCommandBullets" ~: parseShopCommand "3" ~?= Just R.Bullets,
  "testParseShopCommandOxen" ~: parseShopCommand "4" ~?= Just R.Oxen,
  "testParseShopCommandMedicine" ~: parseShopCommand "5" ~?= Just R.Medicine,
  "testParseShopCommandInvalid" ~: parseShopCommand "invalid" ~?= Nothing
  ]

-- Run all the tests.
tests :: Test
tests =
  TestList
    [ testParseCommand,
      testParseInt,
      testParseTownCommand,
      testParseProfession,
      testParseShopCommand
    ]

runTest :: IO ()
runTest = runTestTT tests >>= print