-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Parser
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The Holumbus query parser, based on the famous Parsec library.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Parser (parseQuery) where

import Holumbus.Query.Syntax
import Text.ParserCombinators.Parsec
import Data.Either

-- | Parse a query.
parseQuery :: String -> Either String Query
parseQuery = result . (parse query "")
  where
  result (Left err) = Left (show err)
  result (Right q)  = Right q

-- | A query may always be surrounded by whitespace
query :: Parser Query
query = andQuery

-- | Parse an and query.
andQuery :: Parser Query
andQuery = do t <- orQuery
              spaces
              do andOp
                 spaces
                 q <- query
                 return (BinQuery And t q)
                 <|> return t

-- | Parse an or query.
orQuery :: Parser Query
orQuery = do t <- notQuery
             spaces
             do orOp
                spaces
                q <- orQuery
                return (BinQuery Or t q)
                <|> return t

-- | Parse a negation.
notQuery :: Parser Query
notQuery = do spaces
              notQuery' <|> contextQuery
  where
  notQuery' = do notOp
                 spaces
                 q <- contextQuery
                 return (Negation q)

-- | Parse a context query.
contextQuery :: Parser Query
contextQuery = do try contextQuery' <|> parQuery
  where
  contextQuery' = do c <- contexts
                     spaces
                     char ':'
                     spaces
                     t <- parQuery
                     return (Specifier c t)

-- | Parse a query surrounded by parentheses.
parQuery :: Parser Query
parQuery = do parQuery' <|> caseQuery
  where
  parQuery' = do char '('
                 q <- query
                 char ')'
                 return q

-- | Parse a case sensitive query.
caseQuery :: Parser Query
caseQuery = do caseQuery' <|> fuzzyQuery
  where
  caseQuery' = do char '!'
                  spaces
                  (phraseQuery CasePhrase <|> wordQuery CaseWord)

-- | Parse a fuzzy query.
fuzzyQuery :: Parser Query
fuzzyQuery = do fuzzyQuery' <|> phraseQuery Phrase <|> wordQuery Word
  where
  fuzzyQuery' = do char '~'
                   spaces
                   wordQuery FuzzyWord

-- | Parse a word query.
wordQuery :: (String -> Query) -> Parser Query
wordQuery c = do w <- word
                 return (c w)

-- | Parse a phrase query.
phraseQuery :: (String -> Query) -> Parser Query
phraseQuery c = do p <- phrase
                   return (c p)

-- | Parse an and operator.
andOp :: Parser ()
andOp = do string "AND" 
           return ()
           <|> spaces 

-- | Parse an or operator.
orOp :: Parser ()
orOp = do string "OR"
          return ()

-- | Parse a not operator.
notOp :: Parser ()
notOp = do string "NOT"
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

-- | Parse a list of contexts.
contexts :: Parser [String]
contexts = context `sepBy1` (char ',')
            
-- | Parse a context.
context :: Parser String
context = do spaces 
             c <- (many1 alphaNum)
             spaces
             return c
