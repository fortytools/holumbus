-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.DiffList
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  Providing space efficient difference encoding for lists of integers. For
  convenience, conversion functions for "Data.IntSet" are provided.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Data.DiffList 
  (
  -- * DiffList types
  DiffList
    
  -- * Conversion
  , fromIntSet
  , toIntSet
  , fromList
  , toList
  )
where

import Data.Word
import Data.List

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Holumbus.Data.Crunch

-- | A single difference between two integers.
type Diff = Word64
-- | A list of differences.
type DiffList = [Diff]

-- | Convert a set of integers into a list of difference encoded values.
fromIntSet :: IntSet -> DiffList
fromIntSet = fromList . IS.toAscList

-- | Convert the difference encoded values to a lis of integers.
toIntSet :: DiffList -> IntSet
toIntSet = IS.fromList . toList

-- | Convert a list of integers into a list of difference encoded values.
fromList :: [Int] -> DiffList
fromList = crunch32 . encode . sort

-- | Convert the difference encoded values to a list of integers. The resulting list will be
-- sorted in ascendin
toList :: DiffList -> [Int]
toList = decode . decrunch32

-- | This is were the real work is done: Encoding a sorted list of integers.
encode :: [Int] -> [Word32]
encode = encode' 0
  where
  encode' :: Int -> [Int] -> [Word32]
  encode' _ [] = []
  encode' l (x:xs) = n:(encode' x xs)
    where
    n = fromIntegral (x - l)

-- | This is were the real work is done: Decoding a difference list.
decode :: [Word32] -> [Int]
decode = decode' 0
  where
  decode' :: Int -> [Word32] -> [Int]
  decode' _ [] = []
  decode' l (x:xs) = n:(decode' n xs)
    where
    n = (fromIntegral x) + l 
