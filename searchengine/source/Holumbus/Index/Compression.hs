-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Compression
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module provides several specific compression mechanisms for different
  parts of indexes. Right now, just a general compression scheme for 
  the 'Occurrences' and 'Positions' are provided.

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

-- | Decompressing the occurrences by just decompressing all contained positions.
inflateOcc :: CompressedOccurrences -> Occurrences
inflateOcc = IM.map inflatePos

-- | Compress the occurrences by just compressing all contained positions.
deflateOcc :: Occurrences -> CompressedOccurrences
deflateOcc = IM.map deflatePos

-- | Convert the compressed differences back to a set of integers.
inflatePos :: CompressedPositions -> Positions
inflatePos = DL.toIntSet

-- | Save some memory on the positions by just saving their differences and compressing these.
deflatePos :: Positions -> CompressedPositions
deflatePos = DL.fromIntSet

-- ----------------------------------------------------------------------------
