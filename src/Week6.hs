{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Week6 where

import qualified Data.List as List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
fibs = 0 : ts where
  ts = fst <$> iterate f (1, 0)
  f (x, y) = (x + y, x)

data Stream a = SCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (SCons x xs) = x:streamToList xs

instance Show a => Show (Stream a) where
  show = show . List.take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = SCons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (SCons x xs) = SCons (f x) (streamMap f xs)

streamZip :: Stream a -> Stream b -> Stream (a, b)
streamZip (SCons x xs) (SCons y ys) = SCons (x, y) (streamZip xs ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = SCons x (streamFromSeed f (f x))  

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (SCons x xs) y = SCons x (interleaveStreams y xs)

maxPowers :: Stream Integer
maxPowers = f 0 where
  f n = interleaveStreams (streamRepeat n) (f (n + 1))

x :: Stream Integer
x = SCons 0 (SCons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = SCons n (streamRepeat 0)
  negate = streamMap negate
  a + b = streamMap (uncurry (+)) $ streamZip a b
  a * b = SCons (a0 * b0) xs
    where xs = streamMap (a0 *) bs + (b * as)
          (SCons a0 as) = a
          (SCons b0 bs) = b
  









