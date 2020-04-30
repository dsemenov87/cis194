{-# LANGUAGE MultiWayIf #-}

module Week4 where

import Data.List(filter, map, zip, foldl', foldr, maximum, reverse, sum)
import Data.Bits(xor) 

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl' (\acc x -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 0 = 0
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (div n 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ takeWhile ((/=) 1) $ map fst $ iterate step (step (0,n))
  where step (_,n)
          | n <= 1    = (1, 0)
          | even n    = (n, div n 2)
          | otherwise = (0, 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

ndeep :: Tree a -> Integer
ndeep Leaf = 0
ndeep (Node deep _ _ _) = deep

balance :: Tree a -> Tree a
balance branch =
  case branch of
    Node 2 (Node 1 la za Leaf) z Leaf ->
      Node 1 la za (Node 0 Leaf z Leaf)

    Node 2 Leaf z (Node 1 Leaf za ra) ->
      Node 1 (Node 0 Leaf z Leaf) za ra

    Node 3 (Node 2 Leaf za (Node 1 Leaf zaa Leaf)) z Leaf ->
      Node 2 (Node 1 Leaf za Leaf) zaa (Node 1 Leaf z Leaf)

    Node 2 Leaf z (Node 1 (Node 0 Leaf zaa Leaf) za Leaf) ->
      Node 1 (Node 0 Leaf z Leaf) zaa (Node 0 Leaf za Leaf)

    Node dh (Node dg lg g rg) h (Node de le e re) ->
      if  | dg - de > 1 ->
          case (lg, rg, le, re) of
            (Node da la a ra, Node db lb b rb,_,_) ->
              Node dg (Node da la a ra) g (Node ((max db de) + 1) (Node db lb b rb) h (Node de le e re))

            _ -> branch

          | de - dg > 1 ->
          case (lg, rg, le, re) of
            (_,_,Node dd ld d rd, Node df lf f rf) ->
              Node de (Node ((max dd dg) + 1) (Node dg lg g rg) h (Node dd ld d rd)) e (Node df lf f rf)

            _ -> branch
        
          | otherwise -> branch

    _ -> branch

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node 0 Leaf x Leaf

insert (Node deep ltree y rtree) x  
  | x >= y    = node ltree rtree'
  | otherwise = node ltree' rtree
  where
    node lt rt  = balance $ Node (max (ndeep lt) (ndeep rt) + 1) lt y rt
    ltree'      = insert ltree x
    rtree'      = insert rtree x

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldl' insert Leaf

xor' :: [Bool] -> Bool
xor' = not . foldl' xor True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\i xs -> (f i):xs) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) $ complete $ foldl' f ([], 0) exs
  where 
    f (xs,m) y  = (xs ++ [m+1..y-1], y)
    exs = takeWhile (\x -> x <= n) [(i + j + 2 * i * j) | i <- [1..n], j <- [1..n]]
    complete (xs, m) = xs ++ [m+1..n]

