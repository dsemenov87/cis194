{-# LANGUAGE MultiWayIf, FlexibleInstances #-}

module Week7 where

import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

--joinListFold :: b -> (b -> a -> b -> b) -> JoinList m a -> b 
--joinListFold e _ Empty        = e
--joinListFold e _ (Single m a) = m 
--joinListFold e f (Append m l r) = f (joinListFold e f l) m (joinListFold e f r)

--tag' :: Monoid m => JoinList m a -> m
--tag' = joinListFold mempty (\l _ r -> l <> r)

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m 
tag (Append m l r)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append ((tag l) <> (tag r)) l r

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _    = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0    = Nothing
indexJ i (Single _ a)
  | i == 1 = Just a
  | otherwise = Nothing
indexJ i (Append m l r)
  | si > sm         = Nothing
  | si <= lm        = indexJ i l
  | otherwise       = indexJ (getSize (si - lm)) r
    where lm = size $ tag l
          si = Size i
          sm = size m 

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i <= 0    = jl
dropJ i (Single _ a)   = Empty
dropJ i (Append m l r)
  | si > sm         = Empty
  | si <= lm        = Append m (dropJ i l) r
  | otherwise       = Append m Empty (dropJ (getSize (si - lm)) r)
    where lm = size $ tag l
          si = Size i
          sm = size m 

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0      = Empty
takeJ _ (Single m a)    = Single m a
takeJ i (Append m l r)
  | si > sm         = Append m l r
  | si <= lm        = takeJ i l
  | otherwise       = Append m l (takeJ (getSize (si - lm)) r)
    where lm = size $ tag l
          si = Size i
          sm = size m 

scoreLine :: String -> JoinList Score String
scoreLine line = Single (scoreString line) line  

--JoinList (Score, Size) String







