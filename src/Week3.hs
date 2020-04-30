module Week3 where

import Prelude(Bool(..), Show(..), Eq(..), ($), (.), (&&), (++), (<), (>), (>=), mod, length, fst, snd, flip, Integer, Int, unwords, String, unlines)

import Data.Maybe(Maybe(..))
import Data.List(filter, map, zip, foldl, maximum, reverse)

skips :: [a] -> [[a]]
skips input =
  map (`each` input) numbers
  where
    each n = map snd . filter (\(i,_) -> mod i n == 0) . zip numbers 
    numbers = [1..length input]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:xs) = snd $ foldl f ((x,y),[]) xs where
  f ((x, y), acc) z = ((y, z), if y > x && y > z then acc ++ [y] else acc)
localMaxima _ = []

histogram :: [Integer] -> String
histogram = unlines . draw . entries where
  entries lst = [length $ filter (n ==) lst | n <- [0..9]]
  draw entrs = map (\r -> unwords $ map (symb r) entrs) $ reverse [1..maximum entrs]
  symb r c = if c >= r then "*" else " "
