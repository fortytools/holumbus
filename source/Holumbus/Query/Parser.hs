-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Parser
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The Holumbus query parser. Customized parsers for different query languages
  can also be designed using a parser library like Parsec.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Parser 
  (
  -- * Parser data types
  Parser (P)

  -- * Parsing
  , parseQuery
  , parseQueryWith
  )
where

import Holumbus.Query.Syntax

import Char
import Control.Monad
--import Text.ParserCombinators.Parsec

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

-- | Parse a string with the default parser.
parseQuery :: String -> [(Query, String)]
parseQuery = parse query

-- | Parse a string with a custom parser. This allows for highly customized querylanguages.
parseQueryWith :: Parser Query -> String -> [(Query, String)]
parseQueryWith = parse

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

-- | Parse a single context name.
context :: Parser String
context = token (manyOne alphanum)

-- | Parse a list of context specifiers, which are context names separated by commas.
contexts :: Parser [String]
contexts =  do
              x <- context
              do
                symbol ","
                xs <- contexts
                return (x:xs)
                +++ return [x]

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
                 c <- contexts
                 symbol ":"
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
             +++ caseQuery

-- | Parse a case-sensitive query.
caseQuery :: Parser Query
caseQuery = do
              symbol "!"
              do
                (phraseQuery CasePhrase) +++ (wordQuery CaseWord)
              +++ fuzzyQuery

-- | Parse a fuzzy query.
fuzzyQuery :: Parser Query
fuzzyQuery = do
               symbol "~"
               wordQuery FuzzyWord
               +++ (phraseQuery Phrase) +++ (wordQuery Word)

-- | Parse a terminal word query.
wordQuery :: (String -> Query) -> Parser Query
wordQuery f = do
                w <- word
                return (f w)

-- | Parse a terminal phrase query.
phraseQuery :: (String -> Query) -> Parser Query
phraseQuery f = do
                  p <- phrase
                  return (f p)

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

