-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Index.Inverted
   Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   The inverted index for Spoogle.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Index.Inverted where

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Spoogle.Data.Patricia (Pat)
import qualified Spoogle.Data.Patricia as P

data InvIndex     = InvSpoogle { docTable :: !Documents, indexParts :: !Parts } 
                  deriving (Show)

data Documents     = DocTable { idToDoc :: !(IntMap URL), docToId :: !(Map URL DocId) }
                   deriving (Show)

type Parts      = Map Context Part
type Part       = Pat Occurences

type Occurences = IntMap Positions
type Positions  = IntSet

type DocId      = Int
type Word       = String
type Context    = String
type URL        = String
type Title      = String

empty :: InvIndex
empty = InvSpoogle emptyDocuments M.empty

emptyDocuments :: Documents
emptyDocuments = DocTable IM.empty M.empty


