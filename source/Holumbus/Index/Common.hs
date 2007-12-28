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

module Holumbus.Index.Common where

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

data Documents     = DocTable { idToDoc   :: !(IntMap Document)
                              , docToId   :: !(Map URL DocId) 
                              , lastDocId :: !DocId
                              } deriving (Show)

type Document      = (Title, URL)
type DocId         = Int
type URL           = String
type Title         = String

type Position      = Int
type Context       = String
type Word          = String

-- These are needed for a common result data type (see Holumbus.Query.Result)
type Occurrences   = IntMap Positions    -- The key equals a document id
type Positions     = IntSet              -- The positions of the word in the document

class HolIndex i where
  empty         :: i

  sizeDocs      :: i -> Int
  sizeWords     :: i -> Int
  documents     :: i -> Documents
  contexts      :: i -> [ Context ]

  allWords      :: Context -> i -> [(String, Occurrences)]
  prefixCase    :: Context -> i -> String -> [(String, Occurrences)]
  prefixNoCase  :: Context -> i -> String -> [(String, Occurrences)]
  lookupCase    :: Context -> i -> String -> [Occurrences]
  lookupNoCase  :: Context -> i -> String -> [Occurrences]

  insert        :: Context -> Word -> Position -> Document -> i -> i
  update        :: Context -> Word -> Position -> Document -> i -> i

emptyDocuments :: Documents
emptyDocuments = DocTable IM.empty M.empty 0

emptyOccurrences :: Occurrences
emptyOccurrences = IM.empty

