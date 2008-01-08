-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Syntax
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The Holumbus query syntax definition. The specific syntax of any query
  language can be designed independently by creating appropriate parsers.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Syntax 
  (
  -- * Query data types
  Query (Word, Phrase, CaseWord, CasePhrase, FuzzyWord, Specifier, Negation, BinQuery)
  , BinOp (And, Or)
  )
where

import Holumbus.Index.Common (Context)

-- | The query language.
data Query = Word       String
           | Phrase     String
           | CaseWord   String
           | CasePhrase String
           | FuzzyWord  String
           | Specifier  [Context] Query
           | Negation   Query
           | BinQuery   BinOp Query Query
           deriving (Eq, Show)

-- | A binary operation.
data BinOp = And | Or deriving (Eq, Show)
