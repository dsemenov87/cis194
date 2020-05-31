{-# LANGUAGE MultiWayIf, FlexibleInstances #-}

module Week8 where

import Data.Monoid
import Data.Foldable
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL lst fun) = GL (e:lst) (fun + empFun e)

instance Semigroup GuestList where
    (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2) 

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2) 

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f nod = f (rootLabel nod) (treeFold f <$> subForest nod) 

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e l =
    if empFun e >= f2 - f1
       then (sum, glCons e sum2)
       else (sum, glCons e sum1)
        where sum = max sum1 sum2
              sum1 = foldl' (<>) mempty $ fst <$> l     
              (GL l1 f1) = sum1
              sum2 = foldl' (<>) mempty $ snd <$> l     
              (GL l2 f2) = sum2

maxFun :: Tree Employee -> GuestList
maxFun es = max l1 l2 where 
    (l1, l2) = treeFold nextLevel es
