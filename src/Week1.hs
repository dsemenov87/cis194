module Week1 where

import Prelude(Integer, div, mod, IO, putStrLn, show, (++), ($), (>), (+), (-), (*), (==), reverse, length, sum, Bool(..), String)

toDigits :: Integer -> [Integer]
toDigits num =
  if num > 0 then loop num [] else []
  where loop 0 xs = xs
        loop n xs =
          loop (n `div` 10) ((n `mod` 10):xs) 

toDigitsRev :: Integer -> [Integer]
toDigitsRev num =
  if num > 0 then loop num [] else []
  where loop 0 xs = xs
        loop n xs =
          loop (n `div` 10) (xs ++ [n `mod` 10])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther inp =
  loop False (reverse inp) []
  where loop _ [] out = out
        loop False (x:xs) out = loop True xs (x:out)
        loop True (x:xs) out = loop False xs ((x * 2):out) 
            
sumDigits :: [Integer] -> Integer
sumDigits inp =
  loop inp 0
  where loop [] acc = acc
        loop (x:xs) acc = loop xs (acc + sum (toDigits x))

validateCardNum :: Integer -> Bool
validateCardNum num =
  (res `mod` 10) == 0
  where res = sumDigits $ doubleEveryOther $ toDigits num

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,c)]
hanoi 2 a b c = [(a,b), (a,c), (b,c)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a,c)] ++ hanoi (n - 1) b a c 
