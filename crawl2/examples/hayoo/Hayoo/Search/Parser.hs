-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Parser
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The parser for the Hayoo! web search.
 
-}

-- ----------------------------------------------------------------------------

module Hayoo.Search.Parser where

-- import qualified Data.Map as M
-- import Data.Char

import Hayoo.Signature

-- import Holumbus.Utility
import Holumbus.Query.Language.Grammar

import Text.ParserCombinators.Parsec

-- ------------------------------------------------------------

{- Uwe: done 

-- TODO TH This normalize and strip have to be shared by indexer and search!

-- | Normalizes a Haskell signature, e.g. @String -> Int -> Int@ will be transformed to 
-- @a->b->b@. All whitespace will be removed from the resulting string.
normalizeSignature :: String -> String
normalizeSignature = join "->" . (replaceTypes M.empty ['a'..'z']) . split "->" . filter (not . isSpace)
  where
  replaceTypes _ _ [] = []
  replaceTypes v t (x:xs) = let (nv, ut, rx) = replace' in rx:(replaceTypes nv ut xs)
    where
    replace' = let ut = [head t] in maybe (M.insert r ut v, tail t, ut) (\n -> (v, t, n)) (M.lookup r v)
      where r = stripWith (\c -> (c == '(') || (c == ')')) x

-- | Strip unneeded whitespace from a signature, e.g. @String -> Map k a -> Int@ will be transformed
-- to @String->Map k a->Int@.
stripSignature :: String -> String
stripSignature = sep "->" . lsep "(" . rsep ")" . sep "." . sep "=>"
  where
  sep s = join s . map strip . split s
  lsep s = join s . map stripl . split s
  rsep s = join s . map stripr . split s
-}

-- ------------------------------------------------------------

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
orQuery = do t <- contextQuery
             do orOp
                q <- orQuery
                return (BinQuery Or t q)
                <|> return t

-- | Parse a negation.
notQuery :: Parser Query
notQuery = do notOp
              q <- contextQuery
              return (Negation q)

-- | Parse a context query.
contextQuery :: Parser Query
contextQuery = try contextQuery' <|> parQuery
  where
  contextQuery' = do c <- contexts
                     spaces
                     _ <- char ':'
                     spaces
                     t <- parQuery
                     return (Specifier c t)

-- | Parse a query surrounded by parentheses.
parQuery :: Parser Query
parQuery = parQuery' <|> phraseQuery <|> wordQuery
  where
  parQuery' = do _ <- char '('
                 spaces
                 q <- andQuery
                 spaces
                 _ <- char ')'
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
             _ <- string s
             po <- many1 anyChar
             return (pr ++ s ++ po)

-- | Parse an and operator.
andOp :: Parser ()
andOp = (try andOp') <|> spaces1
  where
  andOp' = do spaces
              _ <- string "AND" 
              spaces1
              return ()

-- | Parse an or operator.
orOp :: Parser ()
orOp = try orOp'
  where
  orOp' = do spaces
             _ <- string "OR"
             spaces1
             return ()

-- | Parse a not operator.
notOp :: Parser ()
notOp = try notOp'
  where
  notOp' = do spaces
              _ <- string "NOT" 
              spaces1
              return ()

-- | Parse a word.
word :: Parser String
word = many1 wordChar

-- | Parse a phrase.
phrase :: Parser String
phrase = do _ <- char '"'
            p <- many1 phraseChar
            _ <- char '"'
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

