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

import Holumbus.Index.Common

data InvIndex    = InvHolumbus { docTable :: !Documents
                               , indexParts :: !Parts 
                               } deriving (Show)

type Parts       = Map Context Part    -- A context has a name and it's own index
type Part        = StrMap Occurrences  -- The word is the key with its occurrences as value

type Occurrences = IntMap Positions    -- The key equals a document id
type Positions   = IntSet              -- The positions of the word in the document

empty :: InvIndex
empty = InvHolumbus emptyDocuments M.empty

emptyOccurrences :: Occurrences
emptyOccurrences = IM.empty

