-- For Advent of Code 2022
--
-- By Truman Collins
-- December 2, 2022

{-# Language LambdaCase #-}

module Parsers (
  Parser,
  parse,
  item,
  sat,
  digit,
  hexDigit,
  lower,
  upper,
  letter,
  alphanum,
  alphanumSp,
  ident,
  identAlpha,
  identWithPeriod,
  identWithSp,
  space,
  newline,
  spaceNotNL,
  char,
  string,
  nat,
  int,
  integerNat,
  integer,
  token,
  identifier,
  natTok,
  intTok,
  integerNatTok,
  integerTok,
  symbol,
  commaIdent,
  cslOfIdents,
  commaNat,
  cslOfNats,
  nats,
  parseNItems,

  validateAndReturnParseResult,
  validateAndReturnParseLinesResult
) where

import Data.Char
import Control.Applicative
import Control.Monad
import Utilities


-- Parser functions.

-- A parser of a given type (a) is a function that takes a string and return of list of pairs of
-- things of type (a) and remaining strings. An empty list indicates the parseing function failed,
-- and a result with more than one pair indicates that there was more than one valid parse of a
-- thing of type (a) at the start of the input string.

-- This is necessary to allow the Parser type to be made into instances of classes. P is a dummy
-- constructor.

newtype Parser a = P (String -> [(a, String)])

-- Using this newtype, a parsing function can be applied to a string using a function that removes
-- the dummy constructor. Before the function can be applied, it needs to be removed from the
-- parser context. Just return the function.

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

-- Define a parser called item which just takes the first character and fails for empty input.

item :: Parser Char
item = P (\case
             []       -> []
             (x : xs) -> [(x, xs)])
--item = P (\str -> case str of
--                    []       -> []
--                    (x : xs) -> [(x, xs)])

-- This makes Parser part of the Functor class.
-- Now: parse (fmap toUpper item) "abc"
--      [('A', "bc")]

instance Functor Parser where
  fmap g p = P(\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> [(g v, out)]
                         _          -> [])  -- This case won't ever happen, but removes warning.

-- Pure just adds its argument to the result in the parse pair.
-- for <*>, this will return the first and third character.
--   three :: Parser (Char, Char)
--   three = pure g <*> item <*> item <*> item
--     where g x y z = (x, z)

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> case parse pg inp of
                           [] -> []
                           [(g, out)] -> parse (fmap g px) out
                           _          -> [])  -- This case won't ever happen, but removes warning.

-- Make this an instance of Monad. Now:
-- three :: Parser (Char, Char)
-- three = do
--   x <- item
--   item
--   z <- item
--   return (x, z)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out
                         _          -> [])  -- This case won't ever happen, but removes warning.

-- This is the alternative class associated with Applicative.

instance Alternative Parser where
  empty = P (const [])
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v,out)]
                         _          -> [])  -- This case won't ever happen, but removes warning.

-- Define a parser that succeeds for a single character if it
-- satisfies the given predicate.
-- For example:
-- digit :: Parser Char
-- digit = sat isDigit

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- Now some basic parsers:

digit :: Parser Char
digit = sat isDigit

hexDigit :: Parser Char
hexDigit = sat isHexDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- Parse an alpha-numeric character or space.

alphanumSp :: Parser Char
alphanumSp = sat isAlphaNum <|> sat (== ' ')

-- Parse an alpha-numeric character or period.

alphanumPeriod :: Parser Char
alphanumPeriod = sat isAlphaNum <|> sat (== '.')

-- Parse an identifier beginning with a letter and alpha-numeric after that.

ident :: Parser String
ident = do
  x <- letter
  xs <- many alphanum
  return (x : xs)

-- Parse an identifier with only letters.

identAlpha :: Parser String
identAlpha = do
  x <- letter
  xs <- many letter
  return (x : xs)

-- Similar to the above, but it can start with a number too and have spaces in it.

identWithSp :: Parser String
identWithSp = do
  x <- alphanum
  xs <- many alphanumSp
  return (x : xs)

-- Similar to the above, but it can contail periods as well.

identWithPeriod :: Parser String
identWithPeriod = do
  x <- alphanumPeriod
  xs <- many alphanumPeriod
  return (x : xs)

-- Consume spaces.

space :: Parser ()
space = do
  _ <- many (sat isSpace)
  return ()

-- Consume one or more newline characters.

newline :: Parser ()
newline = do
  _ <- many (sat (== '\n'))
  return ()

-- Consume whitespace except newlines. Return the number of characters parsed.

spaceNotNL :: Parser Int
spaceNotNL = do
  spaces <- many (sat isSpaceNotNL)
  return (length spaces)
  where
    isSpaceNotNL :: Char -> Bool
    isSpaceNotNL ch
      | isSpace ch && ch /= '\n' = True
      | otherwise = False

-- Match a specific character in the input.

char :: Char -> Parser Char
char x = sat (== x)

-- Match a specific string in the input.

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x:xs)

-- Parse and return a natural number.

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- Parse and return an integer with optional negative sign.

int :: Parser Int
int = do
    _ <- char '-'
    n <- nat
    return (-n)
  <|>
    nat

-- Parse and return a natural number as an Integer.

integerNat :: Parser Integer
integerNat = do
  xs <- some digit
  return (read xs)

-- Parse and return an integer with optional negative sign.

integer :: Parser Integer
integer = do
    _ <- char '-'
    n <- integerNat
    return (-n)
  <|>
    integerNat

-- Given a parser, return a new one that consumes any whitespace before and after the thing parsed.

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

-- Tokenized things.

identifier :: Parser String
identifier = token ident

natTok :: Parser Int
natTok = token nat

intTok :: Parser Int
intTok = token int

integerNatTok :: Parser Integer
integerNatTok = token integerNat

integerTok :: Parser Integer
integerTok = token integer

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Parse a comma followed by an identifier, and return that string.

commaIdent :: Parser String
commaIdent = do
  _ <- symbol ","
  identifier

-- Parse a comma-separated list of identifiers and return the list of them.

cslOfIdents :: Parser [String]
cslOfIdents = do
  n  <- identifier
  ns <- many commaIdent
  return (n : ns)

-- Parse a comma followed by a natural number, and return that number.

commaNat :: Parser Int
commaNat = do
  _ <- symbol ","
  _ <- space
  natTok

-- Parse a comma-separated list of natural numbers and return the list of them.

cslOfNats :: Parser [Int]
cslOfNats = do
  n  <- natTok
  ns <- many commaNat
  return (n : ns)

-- To parse a list of comma-separated numbers with the whole list in brackets:
-- parse nats "  [1, 2, 3 ] "
-- [([1,2,3], "")]

nats :: Parser [Int]
nats = do
  _ <- symbol "["
  ns <- cslOfNats
  _ <- symbol "]"
  return ns

-- Parse n items defined by the parser passed in, and return the in an in-order list.

parseNItems :: Int -> Parser a -> Parser [a]
parseNItems 0 _ = return []
parseNItems n parseFn = do
  thingParsed <- parseFn
  remainingThings <- parseNItems (n - 1) parseFn
  return (thingParsed : remainingThings)

-- Test the parse result for errors, and if there are any, report an error and stop.
-- If the result is okay, then return it.

validateAndReturnParseResult :: [(a, String)] -> String -> Int -> IO a
validateAndReturnParseResult xs inputFile puzzNum = do
  let badParseResult = null xs || (not . null . tail) xs || (not . null . snd . head) xs

  when badParseResult
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Unable to parse input."))

  return ((fst . head) xs)
  
-- Similar to the above function, but here the parse was done on a list of lines,
-- so there is a list of parse results and if validated, a list of results is returned.

validateAndReturnParseLinesResult :: [[(a, String)]] -> String -> Int -> IO [a]
validateAndReturnParseLinesResult xs inputFile puzzNum = do
  let badParseResult = any null xs || (not . all (null . tail)) xs
        || (not . all (null . snd . head)) xs

  when badParseResult
       (ioError $ userError (genErrStrForIOErrorFn puzzNum inputFile "Unable to parse input."))

  return (map (fst .head) xs)
