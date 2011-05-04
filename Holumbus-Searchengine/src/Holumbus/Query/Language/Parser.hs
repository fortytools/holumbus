{-# OPTIONS_GHC  -fno-warn-unused-do-bind #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Language.Parser
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The Holumbus query parser, based on the famous Parsec library.

  The parser implements a default syntax for the query grammar which exposes
  all possible query types and operators to the user.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Language.Parser
  (
  -- * Parsing
  parseQuery
  )
where

import Holumbus.Query.Language.Grammar
import Text.ParserCombinators.Parsec

-- ----------------------------------------------------------------------------

-- | Parse a query using the default syntax provided by the Holumbus framework.
parseQuery :: String -> Either String Query
parseQuery = result . (parse query "")
  where
  result (Left err) = Left (show err)
  result (Right q)  = Right q

-- | A query may always be surrounded by whitespace
query :: Parser Query
query = spaces >> andQuery

-- | Parse an and query.
andQuery :: Parser Query
andQuery = do t <- orQuery
              try (andOp' t) <|> return t
  where
  andOp' r = do andOp
                q <- andQuery
                return (BinQuery And r q)

-- | Parse an or query.
orQuery :: Parser Query
orQuery = do t <- notQuery
             do orOp
                q <- orQuery
                return (BinQuery Or t q)
                <|> return t

-- | Parse a negation.
notQuery :: Parser Query
notQuery = do notQuery' <|> contextQuery
  where
  notQuery' = do notOp
                 q <- contextQuery
                 return (Negation q)

-- | Parse a context query.
contextQuery :: Parser Query
contextQuery = try contextQuery' <|> parQuery
  where
  contextQuery' = do c <- contexts
                     spaces
                     char ':'
                     spaces
                     t <- parQuery
                     return (Specifier c t)

-- | Parse a query surrounded by parentheses.
parQuery :: Parser Query
parQuery = parQuery' <|> caseQuery
  where
  parQuery' = do char '('
                 spaces
                 q <- andQuery
                 spaces
                 char ')'
                 return q

-- | Parse a case-sensitive query.
caseQuery :: Parser Query
caseQuery = caseQuery' <|> fuzzyQuery
  where
  caseQuery' = do char '!'
                  spaces
                  (phraseQuery CasePhrase <|> wordQuery CaseWord)

-- | Parse a fuzzy query.
fuzzyQuery :: Parser Query
fuzzyQuery = fuzzyQuery' <|> phraseQuery Phrase <|> wordQuery Word
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

-- | Parse a list of contexts.
contexts :: Parser [String]
contexts = context `sepBy1` (char ',')

-- | Parse a context.
context :: Parser String
context = do spaces
             c <- (many1 alphaNum)
             spaces
             return c

-- | Parse at least on white space character.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- ------------------------------------------------------------