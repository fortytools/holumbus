-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Compression
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module provides several specific compression mechanisms for different
  parts of indexes. Right now, just a general compression scheme for 
  the 'Occurrences' and 'Positions' is provided.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Compression 
  (
  -- * Compression types
  CompressedOccurrences
  , CompressedPositions
  
  -- * Compress
  , deflateOcc
  , deflatePos
  
  -- * Decompress
  , inflateOcc
  , inflatePos
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Holumbus.Data.DiffList (DiffList)
import qualified Holumbus.Data.DiffList as DL

import Holumbus.Index.Common

type CompressedOccurrences = IntMap CompressedPositions
type CompressedPositions = DiffList

-- | Convert the differences back to a set of integers.
inflateOcc :: CompressedOccurrences -> Occurrences
inflateOcc = IM.map inflatePos

-- | Save some memory on the positions by just saving their differences.
deflateOcc :: Occurrences -> CompressedOccurrences
deflateOcc = IM.map deflatePos

inflatePos :: CompressedPositions -> Positions
inflatePos = DL.toIntSet

deflatePos :: Positions -> CompressedPositions
deflatePos = DL.fromIntSet
