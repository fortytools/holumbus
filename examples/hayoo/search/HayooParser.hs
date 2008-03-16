-- ----------------------------------------------------------------------------

{- |
  Module     : HayooParser
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The parser for the Hayoo! web search.
 
-}

-- ----------------------------------------------------------------------------

module HayooParser where

import HayooHelper

import Holumbus.Query.Language.Grammar
import Text.ParserCombinators.Parsec
import Data.Either

-- | Parse a query using the special Hayoo! syntax.
parseQuery :: String -> Either String Query
parseQuery = result . (parse query "")
  where
  result (Left err) = Left (show err)
  result (Right q)  = Right q

-- | A query may always be surrounded by whitespace
query :: Parser Query
query = spaces >> ((try sigQuery) <|> (andQuery))

-- | Parse an and query.
andQuery :: Parser Query
andQuery = do t <- orQuery
              try (andOp' t) <|> return t
  where
  andOp' r = do andOp
                q <- (notQuery <|> andQuery)
                return (BinQuery And r q)

-- | Parse an or query.
orQuery :: Parser Query
orQuery = do t <- parQuery
             do orOp
                q <- orQuery
                return (BinQuery Or t q)
                <|> return t

-- | Parse a negation.
notQuery :: Parser Query
notQuery = do notOp
              q <- parQuery
              return (Negation q)

-- | Parse a query surrounded by parentheses.
parQuery :: Parser Query
parQuery = parQuery' <|> phraseQuery <|> wordQuery
  where
  parQuery' = do char '('
                 spaces
                 q <- andQuery
                 spaces
                 char ')'
                 return q

-- | Parse a phrase query.
phraseQuery :: Parser Query
phraseQuery = do p <- phrase
                 return (Phrase p)

-- | Parse a word query.
wordQuery :: Parser Query
wordQuery = do w <- word
               return (FuzzyWord w)

-- | Parse a signature.
sigQuery :: Parser Query
sigQuery = do
           r <- contains "->"
           s <- return (stripSignature r)
           n <- return (normalizeSignature r)
           return $ BinQuery Or (Specifier ["signature"] (Word s)) (Specifier ["normalized"] (Word n))

contains :: String -> Parser String
contains s = do
             pr <- many1 (noneOf s)
             string s
             po <- many1 anyChar
             return (pr ++ s ++ po)

-- | Parse an and operator.
andOp :: Parser ()
andOp = (try andOp') <|> spaces1
  where
  andOp' = do spaces
              string "AND" 
              spaces1
              return ()

-- | Parse an or operator.
orOp :: Parser ()
orOp = try orOp'
  where
  orOp' = do spaces
             string "OR"
             spaces1
             return ()

-- | Parse a not operator.
notOp :: Parser ()
notOp = try notOp'
  where
  notOp' = do spaces
              string "NOT" 
              spaces1
              return ()

-- | Parse a word.
word :: Parser String
word = many1 wordChar

-- | Parse a phrase.
phrase :: Parser String
phrase = do char '"'
            p <- many1 phraseChar
            char '"'
            return p

-- | Parse a character of a word.
wordChar :: Parser Char
wordChar = noneOf "\")( "

-- | Parse a character of a phrases.
phraseChar :: Parser Char
phraseChar = noneOf "\""

-- | Parse at least on white space character.
spaces1 :: Parser ()
spaces1 = skipMany1 space
