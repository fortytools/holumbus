-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Inverted
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  The inverted index for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Inverted where

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

data InvIndex    = InvHolumbus { docTable :: !Documents, indexParts :: !Parts } 
                 deriving (Show)

data Documents   = DocTable { idToDoc :: !(IntMap Document), docToId :: !(Map URL DocId) }
                 deriving (Show)

type Parts       = Map Context Part    -- A context has a name and it's own index
type Part        = StrMap Occurrences  -- The word is the key with its occurrences as value

type Occurrences = IntMap Positions    -- The key equals a document id
type Positions   = IntSet              -- The positions of the word in the document

type Document    = (Title, URL)

type DocId       = Int
type Position    = Int
type Word        = String
type Context     = String
type URL         = String
type Title       = String

empty :: InvIndex
empty = InvHolumbus emptyDocuments M.empty

emptyDocuments :: Documents
emptyDocuments = DocTable IM.empty M.empty

emptyOccurrences :: Occurrences
emptyOccurrences = IM.empty

