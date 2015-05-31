module Main where

import Test.HUnit

import Golf

main :: IO ()
main = runTestTT tests >>= print

tests = TestList $ hopscotchTests ++ lmTests ++ histTests

hopscotchTests = [ "testHopString1" ~: testHopString1
                 , "testHopString2" ~: testHopString2
                 , "testHopSingle"  ~: testHopSingle
                 , "testHopBool"    ~: testHopBool
                 , "testHopEmpty"   ~: testHopEmpty
                 ]

lmTests = [ "testLM1" ~: testLM1
          , "testLM2" ~: testLM2
          , "testLM3" ~: testLM3
          ]

histTests = [ "testHist1" ~: testHist1
            ]

testHopString1 = skips "ABCD"       ~?= ["ABCD", "BD", "C", "D"]
testHopString2 = skips "hello!"     ~?= ["hello!", "el!", "l!", "l", "o", "!"]
testHopSingle  = skips [1]          ~?= [[1]]
testHopBool    = skips [True,False] ~?= [[True,False], [False]]
testHopEmpty   = skips ([] :: [()]) ~?= [] -- Remove ambiguous type

testLM1 = localMaxima [2,9,5,6,1] ~?= [9,6]
testLM2 = localMaxima [2,3,4,1,5] ~?= [4]
testLM3 = localMaxima [1,2,3,4,5] ~?= []

testHist1 = histogram [3,5] ~?= "   * *    \n==========\n0123456789\n"



