{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse fileContents = parseMessage <$> lines fileContents

parseMessage :: String -> LogMessage
parseMessage line =
  case take 1 line of
    "E" -> LogMessage (Error (read (words line !! 1) :: Int)) (parseTimeStamp line) (parseMessageString line)
    "I" -> LogMessage Info (parseTimeStamp line) (parseMessageString line)
    "W" -> LogMessage Warning (parseTimeStamp line) (parseMessageString line)
    _   -> Unknown line

parseTimeStamp :: String -> TimeStamp
parseTimeStamp line =
  case take 1 line of
    "E" -> read (words line !! 2) :: Int
    _   -> read (words line !! 1) :: Int

parseMessageString :: String -> String
parseMessageString line =
  case take 1 line of
    "E" -> unwords . drop 3 $ words line
    _   -> unwords . drop 2 $ words line

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert msgToInsert@(LogMessage _ insertingTimeStamp _) msgTree =
  case msgTree of
    Leaf -> Node Leaf msgToInsert Leaf
    Node leftMsgTree currentMsg@(LogMessage _ currentTimeStamp _) rightMsgTree ->
      if currentTimeStamp > insertingTimeStamp then Node (insert msgToInsert leftMsgTree) currentMsg rightMsgTree
                                               else Node leftMsgTree currentMsg (insert msgToInsert rightMsgTree)
    Node _ (Unknown _) _ -> msgTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node leftMsgTree msg rightMsgTree) = inOrder leftMsgTree ++ [msg] ++ inOrder rightMsgTree
inOrder Leaf = []

-- extract errors with a severity of >= 50
-- takes UNSORTED list of LogMessage's
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  extractMsgText $ filterSevereErrors $ inOrder $ build msgs

filterSevereErrors :: [LogMessage] -> [LogMessage]
filterSevereErrors [] = []
filterSevereErrors msg@(LogMessage (Error severity) _ _:rest)
  | severity >= 50 = head msg : filterSevereErrors rest
  | otherwise      = filterSevereErrors rest
filterSevereErrors (LogMessage{}:rest) = filterSevereErrors rest
filterSevereErrors (Unknown{}:rest) = filterSevereErrors rest

extractMsgText :: [LogMessage] -> [String]
extractMsgText [] = []
extractMsgText (LogMessage _ _ msgText:rest) = msgText : extractMsgText rest
extractMsgText (Unknown _:rest) = extractMsgText rest
