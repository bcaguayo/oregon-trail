module TraceT where

import Trace
import Test.HUnit
    ( assertBool, assertEqual, runTestTT, Test(TestList, TestCase) )

{-
test cases:

1. just quitting should return byebye
2. setting out without food should end the game
3. buying more than allowed should return error
-}