-- ----------------------------------------------------------------------------

{- |
  Module     : Spoogle.Query.Parser
  Copyright  : Copyright (C) 2007 Timo B. Hübel
  License    : MIT

  Maintainer : Timo B. Hübel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : $Id$

  The Spoogle query parser.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Query.Parser 
  (
  -- * Query data types
  Query (Word, Phrase, Specifier, Negation, BinQuery), 
  BinOp (And, Or),

  -- * Parsing
  parseQuery
  )
where

import Spoogle.Index.Inverted (Context)

import Char
import Control.Monad

-- | The query datastructure.
data Query = Word      String
           | Phrase    String
           | Specifier Context Query
           | Negation  Query
           | BinQuery  BinOp Query Query
           deriving (Eq, Show)

-- | A binary operation.
data BinOp = And | Or deriving (Eq, Show)

-- | The parser monad.
data Parser a = P (String -> [(a, String)])

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f  = P (\inp -> case parse p inp of
                         [] -> []
                         (v, out):_ -> (parse (f v) out))

-- | The choice operator, read as "or else".
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P( \inp -> case parse p inp of
                       [] -> parse q inp
                       (v, out):_ -> [(v, out)])

-- | Just for convenience: Apply the query parser to a string.
parseQuery :: String -> [(Query, String)]
parseQuery q = parse query q

-- | Apply a parser to an input string.
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- | The failure parser, always returns a failure.
failure :: Parser a
failure = P (\_ -> [])

-- | Parse a single item (a character).
item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)])

-- | Apply some predicate to an item.
sat :: (Char -> Bool) -> Parser Char
sat p = do
          x <- item
          if (p x) then return x else failure

-- | Parse an alphanumeric character.
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- | Parse a character that is valid in a query (not one of the reserved symbols).
querychar :: Parser Char
querychar = sat isQueryChar

-- | Parse a character that is valid in a phrase (everything except the phrase delimiter).
phrasechar :: Parser Char
phrasechar = sat isPhraseChar

-- | Is a character valid in a phrase (i.e. not the phrase delimiter)?
isPhraseChar :: Char -> Bool
isPhraseChar c | c == '"'  = False
               | otherwise = True

-- | Is a character valid in a query (i.e. not one of the reserved symbols)?
isQueryChar :: Char -> Bool
isQueryChar c | c == '"'   = False
              | c == '('   = False
              | c == ')'   = False
              | c == ':'   = False
              | isSpace c  = False
              | otherwise  = True

-- | Parse a specific character.
char :: Char -> Parser Char
char x = sat (== x)

-- | Parse a string (a sequence of characters).
string :: String -> Parser String
string [] = return []
string (x:xs) = do
                  char x
                  string xs
                  return (x:xs)

-- | Apply a parser several times in sequence.
many :: Parser a -> Parser [a]
many p = manyOne p +++ return []

-- | Apply a parser several times but at leas one time in sequence.
manyOne :: Parser a -> Parser [a]
manyOne p = do 
              v <- p
              vs <- many p
              return (v:vs)

-- | Parse a abritrary amount of whitespace.
space :: Parser ()
space = do
          many (sat isSpace)
          return ()

-- | Parse a token, which is something not equal to whitespace that may be surrounded by any whitespace.
token :: Parser a -> Parser a
token p = do
            space
            v <- p
            space
            return v

-- | Parse a symbol, which is a specific string treated as token (may be surrounded by whitespace).
symbol :: String -> Parser String
symbol xs = token (string xs)

-- | Parse a context specifier, which is a context name followed by a colon.
context :: Parser String
context = token context'
  where
    context' :: Parser String
    context' = do
                 x <- manyOne alphanum
                 char ':'
                 return x

-- | Parse a phrase, which is anything surrounded by the phrase delimiters.
phrase :: Parser String
phrase = token phrase'
  where
    phrase' :: Parser String
    phrase' = do
                char '"'
                x <- manyOne phrasechar
                char '"'
                return x

-- | Parse a single word, which may not contain reserved symbols.
word :: Parser String
word = token word'
  where  
    word' :: Parser String
    word' = do
              x <- manyOne querychar
              return x

-- | Just for convenience, the basic query consists of and terms.
query :: Parser Query
query = andQuery

-- | Parse an AND-query.
andQuery :: Parser Query
andQuery = do
             t <- orQuery
             do
               andOp
               q <- query
               return (BinQuery And t q)
               +++ return t

-- | Parse an OR-query.
orQuery :: Parser Query
orQuery = do
            t <- notQuery
            do
              orOp
              q <- query
              return (BinQuery Or t q)
              +++ return t

-- | Parse a NOT-query.
notQuery :: Parser Query
notQuery = do
             notOp
             q <- contextQuery
             return (Negation q)
             +++ contextQuery

-- | Parse a context-query.
contextQuery :: Parser Query
contextQuery = do
                 c <- context
                 t <- parQuery
                 return (Specifier c t)
                 +++ parQuery

-- | Parse a query enclosed in round brackets.
parQuery :: Parser Query
parQuery = do
             symbol "("
             q <- query
             symbol ")"
             return q
             +++ wordQuery
             +++ phraseQuery

-- | Parse a single word-query.
wordQuery :: Parser Query
wordQuery = do
              w <- word
              return (Word w)

-- | Parse a single phrase-query.
phraseQuery :: Parser Query
phraseQuery = do
                p <- phrase
                return (Phrase p)

-- | Parse an AND-operator ("AND" or just whitespace).
andOp :: Parser ()
andOp = do
        symbol "AND"
        return ()
        +++ space

-- | Parse an OR-operator ("OR").
orOp :: Parser ()
orOp = do
         symbol "OR"
         return ()

-- | Parse a NOT-operator ("NOT").
notOp :: Parser ()
notOp = do
          symbol "NOT"
          return ()

