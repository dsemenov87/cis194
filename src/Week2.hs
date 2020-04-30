{-# OPTIONS_GHC -Wall #-}
module Week2 where

import Prelude(Show(..), Eq(..), ($), words, unwords, String, IO, FilePath, Int, lines, take, (.), read, readFile, (++), (>=))

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parser n file = take n . parser <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parser whatWentWrong file
  = whatWentWrong . parser <$> readFile file

parseMessage :: String -> LogMessage
parseMessage =
  head . words
  where head ("I":xs) = time Info xs
        head ("W":xs) = time Warning xs
        head ("E":x:xs) = time (Error $ read x) xs
        head _ = Unknown "Unknown log format"

        time _ [] = Unknown "Unknown log format"
        time typ (x:xs) = LogMessage typ (read x) (unwords xs)

parse :: String -> [LogMessage]
parse = loop . lines
  where loop [] = []
        loop (x:xs) = parseMessage x:(loop xs) 

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert _ (Node ltree (Unknown utxt) rtree) = Node ltree (Unknown utxt) rtree
insert (LogMessage mtype time blabla) (Node ltree (LogMessage nmtype ntime nblabla) rtree) =
  if time >= ntime
    then Node ltree (LogMessage nmtype ntime nblabla) (insert (LogMessage mtype time blabla) rtree)
    else Node (insert (LogMessage mtype time blabla) ltree) (LogMessage nmtype ntime nblabla) rtree 

build :: [LogMessage] -> MessageTree
build msgs =
  add msgs Leaf
  where add [] tree = tree
        add (x:xs) tree = add xs (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  sort tree
  where sort Leaf = []
        sort (Node ltree msg rtree) = (sort ltree) ++ [msg] ++ (sort rtree)

