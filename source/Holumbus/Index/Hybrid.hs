-- ----------------------------------------------------------------------------

{- |
  Module     : Spoogle.Index.Hybrid
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The hybrid index for Spoogle.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Index.Hybrid where

import Data.Map
import Data.IntMap

import Spoogle.Data.StrMap

data HybIndex      = HybSpoogle { docTable :: !Documents, indexParts :: !Parts } 
                   deriving (Show)

data Documents     = DocTable { idToDoc :: !(IntMap URL), docToId :: !(Map URL DocId) }
                   deriving (Show)

type Parts         = Map Context Part
type Part          = ( Dictionary, Blocks )

type Dictionary    = StrMap WordInfo
type WordInfo      = ( WordId, BlockId )          -- BlockID ersetzt Range-Info

type Blocks        = IntMap Block                 -- Dient nur zum schnellen Finden eines Blocks
type Block         = [ Occurrence ]               -- Sortiert nach DocID
type Occurrence    = ( DocId, WordId, Position )

type DocId         = Int
type WordId        = Int
type BlockId       = Int
type Position      = Int
type URL           = String
type Context       = String
