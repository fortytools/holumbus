-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Result
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The data type for results of Holumbus queries.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Result 
  (
  -- * Result data types
  Result
  , ContextResult (Res, hits, hints)
  , WordHints
  , DocHits
  , WordHits
  
  -- * Construction
  , emptyResult
  , emptyContextResult
  , emptyDocHits
  , emptyWordHits
  , emptyWordHints
  )
where

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Holumbus.Index.Common

type Result = Map Context ContextResult

data ContextResult = Res { hits :: !DocHits, hints :: !WordHints} deriving (Show)

type DocHits = IntMap WordHits          -- Key is document id
type WordHits = Map String Positions
type WordHints = Map String Occurrences

emptyDocHits :: DocHits
emptyDocHits = IM.empty

emptyWordHits :: WordHits
emptyWordHits = M.empty

emptyWordHints :: WordHints
emptyWordHints = M.empty

emptyContextResult :: ContextResult
emptyContextResult = Res emptyDocHits emptyWordHints

emptyResult :: Result
emptyResult = M.empty
