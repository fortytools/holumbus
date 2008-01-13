-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Common data types shared by all index types and a unified interface for
  all different index types.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common 
  (
  -- * Common index types and classes
  Position
  , Context
  , Word
  , Occurrences
  , Positions
  , HolIndex (..)

  -- * Construction
  , emptyOccurrences

  -- * Pickling
  , xpOccurrences
  , xpPositions
  )
where

import Text.XML.HXT.Arrow.Pickle

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Holumbus.Index.Documents (Documents, Document)
import qualified Holumbus.Index.Documents as D

-- | The position of a word in the document.
type Position      = Int
-- | The name of a context.
type Context       = String
-- | A single word.
type Word          = String

-- | The occurrences in a number of documents. A mapping from document ids to the positions in the document.
type Occurrences   = IntMap Positions
-- | The positions of the word in the document.
type Positions     = IntSet

-- | This class provides a generic interface to different types of index implementations.
class HolIndex i where
  -- | Returns the number of unique documents in the index.
  sizeDocs      :: i -> Int
  -- | Returns the number of unique words in the index.
  sizeWords     :: i -> Int
  -- | Returns the table for mapping between documents and their ids.
  documents     :: i -> Documents
  -- | Returns a list of all contexts avaliable in the index.
  contexts      :: i -> [ Context ]

  -- | Returns the occurrences for every word. A potentially expensive operation.
  allWords      :: Context -> i -> [(String, Occurrences)]
  -- | Searches for words beginning with the prefix in a given context (case-sensitive).
  prefixCase    :: Context -> i -> String -> [(String, Occurrences)]
  -- | Searches for words beginning with the prefix in a given context (case-insensitive).
  prefixNoCase  :: Context -> i -> String -> [(String, Occurrences)]
  -- | Searches for and exact word in a given context (case-sensitive).
  lookupCase    :: Context -> i -> String -> [Occurrences]
  -- | Searches for and exact word in a given context (case-insensitive).
  lookupNoCase  :: Context -> i -> String -> [Occurrences]

  -- | Inserts an occurrence of a word for a given context.
  insert        :: Context -> Word -> Position -> Document -> i -> i
  -- | Updates an occurrence of a word for a given context.
  update        :: Context -> Word -> Position -> Document -> i -> i

-- | The XML pickler for a set of positions.
xpPositions :: PU Positions
xpPositions = xpWrap ( IS.fromList . (map read) . words
                     , unwords . (map show) . IS.toList
                     ) xpText

-- | The XML pickler for the occurrences of a word.
xpOccurrences :: PU Occurrences
xpOccurrences = xpWrap (IM.fromList, IM.toList) (xpList xpOccurrence)
  where
  xpOccurrence = xpElem "doc" (xpPair (xpAttr "idref" xpPrim) xpPositions)

-- | Create an empty set of positions.
emptyOccurrences :: Occurrences
emptyOccurrences = IM.empty
