-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Language
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The Holumbus query language definition. The specific syntax of any query
  language can be designed independently by creating appropriate parsers.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Language
  (
  -- * Query data types
  Query (Word, Phrase, CaseWord, CasePhrase, FuzzyWord, Specifier, Negation, BinQuery)
  , BinOp (And, Or, But)

  -- * Optimizing
  , optimize
  , checkWith
  )
where

import Data.Binary
import Control.Monad

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
data BinOp = And | Or | But deriving (Eq, Show)

instance Binary Query where
  put (Word s)           = put (0 :: Word8) >> put s
  put (Phrase s)         = put (1 :: Word8) >> put s
  put (CaseWord s)       = put (2 :: Word8) >> put s
  put (CasePhrase s)     = put (3 :: Word8) >> put s
  put (FuzzyWord s)      = put (4 :: Word8) >> put s
  put (Specifier c q)    = put (5 :: Word8) >> put c >> put q
  put (Negation q)       = put (6 :: Word8) >> put q
  put (BinQuery o q1 q2) = put (7 :: Word8) >> put o >> put q1 >> put q2

  get = do tag <- getWord8
           case tag of
             0 -> liftM Word get
             1 -> liftM Phrase get
             2 -> liftM CaseWord get
             3 -> liftM CasePhrase get
             4 -> liftM FuzzyWord get
             5 -> liftM2 Specifier get get
             6 -> liftM Negation get
             7 -> liftM3 BinQuery get get get
             _ -> fail "Error while decoding Query"   

instance Binary BinOp where
  put And = put (0 :: Word8)
  put Or  = put (1 :: Word8)
  put But = put (2 :: Word8)

  get = do tag <- getWord8
           case tag of
             0 -> return And
             1 -> return Or
             2 -> return But
             _ -> fail "Error while decoding BinOp"

-- | Transforms all @(BinQuery And q1 q2)@ where one of @q1@ or @q2@ is a @Negation@ into
-- @BinQuery Filter q1 q2@ or @BinQuery Filter q2 q1@ respectively.
optimize :: Query -> Query
--optimize (BinQuery And (Word q1) (Word q2)) = 
--optimize (BinQuery And (CaseWord q1) (CaseWord q2)) =
optimize (BinQuery And q1 (Negation q2)) = BinQuery But (optimize q1) (optimize q2)
optimize (BinQuery And (Negation q1) q2) = BinQuery But (optimize q2) (optimize q1)
optimize (BinQuery And q1 q2) = BinQuery And (optimize q1) (optimize q2)
optimize (BinQuery Or q1 q2) = BinQuery Or (optimize q1) (optimize q2)
optimize (BinQuery But q1 q2) = BinQuery Or (optimize q1) (optimize q2)
optimize (Negation q) = Negation (optimize q)
optimize (Specifier cs q) = Specifier cs (optimize q)
optimize q = q

-- | Check if the query complies with some custom predicate.
checkWith :: (String -> Bool) -> Query -> Bool
checkWith f (Word s) = f s
checkWith f (Phrase s) = f s
checkWith f (CaseWord s) = f s
checkWith f (CasePhrase s) = f s
checkWith f (FuzzyWord s) = f s
checkWith f (Negation q) = checkWith f q
checkWith f (BinQuery _ q1 q2) = (checkWith f q1) && (checkWith f q2)
checkWith f (Specifier _ q) = checkWith f q

