-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Hybrid
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The hybrid index for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Hybrid where

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Index.Common

data HybIndex      = HybHolumbus { docTable   :: !Documents
                                 , indexParts :: !Parts 
                                 } deriving (Show)

type Parts         = Map Context Part
data Part          = HybPart { dictionary :: !Dictionary
                             , blocks     :: !Blocks 
                             } deriving (Show)

data Dictionary    = HybDictionary { wordTable  :: !(StrMap WordInfo)
                                   , lastWordId :: !WordId
                                   } deriving (Show)

type WordInfo      = ( WordId, BlockId )

data Blocks        = HybBlocks { blockTable  :: !(IntMap Block)
                               , lastBlockId :: !BlockId
                               } deriving (Show)
type Block         = [ Occurrence ]               -- Sorted by DocId
type Occurrence    = ( DocId, WordId, Position )

type WordId        = Int
type BlockId       = Int

empty :: HybIndex
empty = HybHolumbus emptyDocuments M.empty

emptyPart :: Part
emptyPart = HybPart emptyDictionary emptyBlocks

emptyDictionary :: Dictionary
emptyDictionary = HybDictionary SM.empty 0

emptyBlocks :: Blocks
emptyBlocks = HybBlocks IM.empty 0
