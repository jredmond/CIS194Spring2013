{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read

-- Exercise 1
-- Parse an individual line of a log file
parseMessage :: String -> LogMessage
parseMessage str =
    case (head . words) str of
        "I" -> parseInfoOrWarningMessage Info $ words str
        "W" -> parseInfoOrWarningMessage Warning $ words str
        "E" -> parseErrorMessage $ words str
        _   -> Unknown str

parseInfoOrWarningMessage :: MessageType -> [String] -> LogMessage
parseInfoOrWarningMessage t totalMes@(_:timeStamp:message) = 
    case (readMaybe timeStamp) :: Maybe TimeStamp of
        Just i  -> LogMessage t i (unwords message)
        Nothing -> Unknown (unwords totalMes)
parseInfoOrWarningMessage _ totalMes = Unknown (unwords totalMes)

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage totalMes@(_:errorCode:timeStamp:message) = 
    case (readMaybe errorCode, readMaybe timeStamp) :: (Maybe Int, Maybe TimeStamp) of
        (Just err, Just time) -> LogMessage (Error err) time (unwords message)
        _                     -> Unknown (unwords totalMes)
parseErrorMessage totalMes = Unknown (unwords totalMes)

-- Parse an entire log file
parse :: String -> [LogMessage]
parse wholeLog = map parseMessage (lines wholeLog)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert insertMsg Leaf = Node Leaf insertMsg Leaf
insert insertMsg (Node left rootMsg right)
    | insertTimestamp <= rootTimestamp = Node (insert insertMsg left) rootMsg right
    | otherwise                        = Node left rootMsg (insert insertMsg right)
  where
    insertTimestamp = msgTimestamp insertMsg 
    rootTimestamp   = msgTimestamp rootMsg

msgTimestamp :: LogMessage -> TimeStamp
msgTimestamp (Unknown _)                = error "There is no timestamp on Unknown message!"
msgTimestamp (LogMessage _ timestamp _) = timestamp

-- Exercise 3
build :: [LogMessage] -> MessageTree
build msgs = foldl (flip insert) Leaf msgs

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getMessage (inOrder (build (filter isRelevant msgs)))

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error severity) _ _) = serverity >= 50
isRelevant _                                 = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg)        = msg

-- Exercise 6
-- Ran out of mustard







