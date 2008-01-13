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
  -- * Optimizing
  , optimize
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
data BinOp = And | Or | Filter deriving (Eq, Show)

-- | Transforms all @(BinQuery And q1 q2)@ where one of @q1@ and @q2@ is @Negation@ into
-- @BinQuery Filter q1 q2@ or @BinQuery Filter q2 q1@ respectively.
optimize :: Query -> Query
optimize (BinQuery And q1 (Negation q2)) = BinQuery Filter (optimize q1) (optimize q2)
optimize (BinQuery And (Negation q1) q2) = BinQuery Filter (optimize q2) (optimize q1)
optimize (BinQuery And q1 q2) = BinQuery And (optimize q1) (optimize q2)
optimize (BinQuery Or q1 q2) = BinQuery Or (optimize q1) (optimize q2)
optimize (BinQuery Filter q1 q2) = BinQuery Or (optimize q1) (optimize q2)
optimize (Negation q) = Negation (optimize q)
optimize (Specifier cs q) = Specifier cs (optimize q)
optimize q = q

