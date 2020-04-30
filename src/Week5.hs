{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-} 

module Week5 where

import Data.List(filter, map, zip, foldl', foldr, maximum, reverse, sum)
import Parser(parseExp)
import qualified StackVM as SVM
import StackVM(Program)

data ExprT  = Lit Integer
            | Add ExprT ExprT
            | Mul ExprT ExprT
            deriving (Show, Eq)

evalEx :: ExprT -> Integer
evalEx (Lit num)      = num
evalEx (Add exL exR)  = (evalEx exL) + (evalEx exR)
evalEx (Mul exL exR)  = (evalEx exL) * (evalEx exR)

evalStr :: String -> Maybe Integer
evalStr = fmap evalEx . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a 
  add :: a -> a -> a
  mul :: a -> a -> a
  eval :: a -> Either String Integer

testExp :: Expr a => String -> Maybe a
testExp str = parseExp lit add mul str 

testInteger str = (testExp str):: Maybe Integer
testBool    str = (testExp str):: Maybe Bool

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul
  eval = Right . evalEx
  
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)
  eval = Right

instance Expr Bool where
  lit x = x > 0

  add = (||)
  mul = (&&)
  
  eval False = Right 0
  eval True  = Right 1

parseEx :: String -> Maybe ExprT
parseEx = parseExp Lit Add Mul

--evalStr' :: (Expr a) => (String -> Maybe a) -> String -> Maybe Integer
--evalStr' p = fmap eval . p 

instance Expr Program where
  lit = pure . SVM.PushI
  
  add prgL prgR = prgL ++ prgR ++ [SVM.Add] 
  mul prgL prgR = prgL ++ prgR ++ [SVM.Mul]

  eval expr = case SVM.stackVM expr of
                Right (SVM.IVal num)  -> Right num
                _                     -> Left "wtf"  

testProgram str  = (testExp str) :: Maybe Program

