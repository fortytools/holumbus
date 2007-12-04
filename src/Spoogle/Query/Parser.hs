-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Query.Parser
   Copyright  : Copyright (C) 2007 Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   The Spoogle query parser.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Query.Parser where

import Char
import Control.Monad

data Query = Word      String
           | Phrase    String
           | Specifier Context Query
           | Negation  Query
           | BinQuery  BinOp Query Query
           deriving (Eq, Show)

data BinOp = And | Or deriving (Eq, Show)

type Context = String

data Parser a = P (String -> [(a, String)])

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f  = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P( \inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp


failure :: Parser a
failure = P (\inp -> [])

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)])

p :: Parser (Char, Char)
p = do
      x <- item
      item
      y <- item
      return (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = do
          x <- item
          if (p x) then return x else failure

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

querychar :: Parser Char
querychar = sat isQueryChar

phrasechar :: Parser Char
phrasechar = sat isPhraseChar

isPhraseChar :: Char -> Bool
isPhraseChar c | c == '"'  = False
               | otherwise = True

isQueryChar :: Char -> Bool
isQueryChar c | c == '"'   = False
              | c == '('   = False
              | c == ')'   = False
              | c == ':'   = False
              | isSpace c  = False
              | otherwise  = True

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
                  char x
                  string xs
                  return (x:xs)

many :: Parser a -> Parser [a]
many p = manyOne p +++ return []

manyOne :: Parser a -> Parser [a]
manyOne p = do 
              v <- p
              vs <- many p
              return (v:vs)

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

symbol :: String -> Parser String
symbol xs = token (string xs)

context :: Parser String
context = token context'
  where
    context' :: Parser String
    context' = do
                 x <- manyOne alphanum
                 char ':'
                 return x

phrase :: Parser String
phrase = token phrase'
  where
    phrase' :: Parser String
    phrase' = do
                char '"'
                x <- manyOne phrasechar
                char '"'
                return x

word :: Parser String
word = token word'
  where  
    word' :: Parser String
    word' = do
              x <- manyOne querychar
              return x

query :: Parser Query
query = andQuery
  
andQuery :: Parser Query
andQuery = do
             t <- orQuery
             do
               space
               q <- query
               return (BinQuery And t q)
               +++ return t

orQuery :: Parser Query
orQuery = do
            t <- notQuery
            do
              symbol "OR"
              q <- query
              return (BinQuery Or t q)
              +++ return t

notQuery :: Parser Query
notQuery = do
             symbol "NOT"
             q <- query
             return (Negation q)
             +++ contextQuery

contextQuery :: Parser Query
contextQuery = do
                 c <- context
                 t <- parQuery
                 return (Specifier c t)
                 +++ parQuery

parQuery :: Parser Query
parQuery = do
             symbol "("
             q <- query
             symbol ")"
             return q
             +++ wordQuery
             +++ phraseQuery

wordQuery :: Parser Query
wordQuery = do
              w <- word
              return (Word w)

phraseQuery :: Parser Query
phraseQuery = do
                p <- phrase
                return (Phrase p)