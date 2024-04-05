{-# LANGUAGE LambdaCase #-}

-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
-- I made some edits to use Maybe instead of lists

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

item :: Parser Char
item =
  P
    ( \case
        [] -> Nothing
        (x : xs) -> Just (x, xs)
    )

-- Sequencing parsers

instance Functor Parser where
  fmap g p =
    P
      ( \inp -> case parse p inp of
          Nothing -> Nothing
          Just (v, out) -> Just (g v, out)
      )

instance Applicative Parser where
  pure v = P (\inp -> Just (v, inp))

  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          Nothing -> Nothing
          Just (g, out) -> parse (fmap g px) out
      )

instance Monad Parser where
  p >>= f =
    P
      ( \inp -> case parse p inp of
          Nothing -> Nothing
          Just (v, out) -> parse (f v) out
      )

-- Making choices

instance Alternative Parser where
  empty = P (const Nothing)

  p <|> q =
    P
      ( \inp -> case parse p inp of
          Nothing -> parse q inp
          Just (v, out) -> Just (v, out)
      )

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- Handling spacing

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

list :: String -> Parser t -> Parser [t]
list sep parseT =
  many
    ( do
        t <- parseT
        symbol sep
        return t
        <|> parseT
    )
