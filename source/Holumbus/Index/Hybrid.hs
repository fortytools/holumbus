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

data HybIndex      = HybIndex { docTable   :: !Documents
                              , indexParts :: !Parts 
                              } deriving (Show)

type Parts         = Map Context Part
data Part          = Part { dictionary :: !Dictionary
                          , blocks     :: !Blocks 
                          } deriving (Show)

data Dictionary    = Dictionary { wordTable  :: !(StrMap WordInfo)
                                , lastWordId :: !WordId
                                } deriving (Show)

type WordInfo      = ( WordId, BlockId )

data Blocks        = Blocks { blockTable  :: !(IntMap Block)
                            , lastBlockId :: !BlockId
                            } deriving (Show)
type Block         = [ Occurrence ]               -- Sorted by DocId
type Occurrence    = ( DocId, WordId, Position )

type WordId        = Int
type BlockId       = Int

empty :: HybIndex
empty = HybIndex emptyDocuments M.empty

emptyPart :: Part
emptyPart = Part emptyDictionary emptyBlocks

emptyDictionary :: Dictionary
emptyDictionary = Dictionary SM.empty 0

emptyBlocks :: Blocks
emptyBlocks = Blocks IM.empty 0
