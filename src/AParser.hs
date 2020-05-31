{- CIS 194 HW 10
   due Monday, 1 April
-}

{-# LANGUAGE MultiWayIf #-}
module AParser where

import Prelude
import           Control.Applicative
import           Control.Monad

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
    --

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where 
   fmap f pa = Parser $ \str -> first f <$> runParser pa str 

instance Applicative Parser where
    pure f = Parser (\str -> Just (f, str))
    Parser fp <*> Parser xp = Parser combine
        where combine = fp >=> (\(f,s) -> first f <$> xp s) 
                
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\d1 _ d2 -> [d1, d2]) <$> posInt <*> (char ' ') <*> posInt

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

intOrUppercase :: Parser ()
intOrUppercase = void $ satisfy isDigit <|> satisfy isUpper

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (<>) <$> alpha <*> alphaNum
    where alpha = oneOrMore $ satisfy isAlpha
          alphaNum = zeroOrMore $ satisfy isAlphaNum

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = spaces *> trimmed <* spaces
    where
        trimmed = (A <$> parseAtom) <|> (Comb <$> parseComb)
        parseAtom = (N <$> posInt) <|> (I <$> ident)
        parseComb = brackets $ zeroOrMore parseSExpr
        brackets p = char '(' *> spaces *> p <* spaces <* char ')'
