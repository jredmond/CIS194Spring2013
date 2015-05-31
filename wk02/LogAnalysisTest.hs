module Main where

import Test.HUnit

import LogAnalysis
import Log

main :: IO ()
main = runTestTT tests >>= print

tests = TestList $ parseTests ++ treeTests

parseTests = [ "testParseErrorMessage"   ~: testParseErrorMessage
             , "testParseInfoMessage"    ~: testParseInfoMessage
             , "testParseUnknowMessage"  ~: testParseUnknowMessage
             , "testParseWarningMessage" ~: testParseWarningMessage
             ]

treeTests = [ "testInsertUnknown"      ~: testInsertUnknown
             , "testInsertUnknown2"    ~: testInsertUnknown2
             , "testInsertEmpty"       ~: testInsertEmpty
             , "testInsertLessThan"    ~: testInsertLessThan
             , "testInsertGreaterThan" ~: testInsertGreaterThan
             , "testInsertEqual"       ~: testInsertEqual
             , "testInOrder"           ~: testInOrder
             , "testMyWhatWentWrong"   ~: testMyWhatWentWrong
             ]


testParseErrorMessage   = parseMessage "E 2 562 help help" ~?= LogMessage (Error 2) 562 "help help"
testParseInfoMessage    = parseMessage "I 29 la la la"     ~?= LogMessage Info 29 "la la la"
testParseUnknowMessage  = parseMessage "Wrong format"      ~?= Unknown "Wrong format"
testParseWarningMessage = parseMessage "W 1 blah blah"     ~?= LogMessage Warning 1 "blah blah"

testInsertUnknown     = insert (Unknown "ah") Leaf       ~?= Leaf
testInsertUnknown2    = insert (Unknown "") (infoTree 5) ~?= infoTree 5
testInsertEmpty       = insert (infoMsg 5) Leaf          ~?= infoTree 5
testInsertLessThan    = insert (infoMsg 1) (infoTree 5)  ~?= Node (infoTree 1) (infoMsg 5) Leaf
testInsertGreaterThan = insert (infoMsg 7) (infoTree 5)  ~?= Node Leaf         (infoMsg 5) (infoTree 7)
testInsertEqual       = insert (infoMsg 5) (infoTree 5)  ~?= Node (infoTree 5) (infoMsg 5) Leaf
testInOrder           = inOrder (Node (infoTree 1) (infoMsg 2) (infoTree 3)) ~?= [infoMsg 1, infoMsg 2, infoMsg 3]
testMyWhatWentWrong   = TestCase $
    testWhatWentWrong parse whatWentWrong "sample.log" >>= \actual ->
    actual @?= expected
  where
    expected = [ "Way too many pickles", "Bad pickle-flange interaction detected", "Flange failed!"]

infoMsg n = LogMessage Info n ""
infoTree n = Node Leaf (LogMessage Info n "") Leaf