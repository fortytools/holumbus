-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.BiMap
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A bijective map based on the Haskell default map.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Data.BiMap
  (
    -- * BiMap type
    BiMap
    
    -- * Query
    , size
    , lookupA
    , lookupB
    
    -- * Construction
    , empty
    , singleton
    , insert
    
    -- * Conversion
    , fromList
    , fromMap
  )
where

import Data.Map (Map)
import qualified Data.Map as M

-- | The BiMap data type from values of a to values of b and vice versa.
data BiMap a b = BiMap { a2b :: !(Map a b)
                       , b2a :: !(Map b a) 
                       } deriving (Show)

-- | Equality of BiMap's.
instance (Eq a, Eq b) => Eq (BiMap a b) where
  bm1 == bm2 = a2b bm1 == a2b bm2 && b2a bm1 == b2a bm2

-- | Returns the number of elements in the BiMap.
size :: BiMap a b -> Int
size = M.size . a2b -- Sizes of both maps should be equal.

-- | Lookup the value b for another value a.
lookupA :: (Ord a) => a -> BiMap a b -> Maybe b
lookupA k m = M.lookup k (a2b m)

-- | Lookup the value a for another value b.
lookupB :: (Ord b) => b -> BiMap a b -> Maybe a
lookupB k m = M.lookup k (b2a m)

-- | Create an empty BiMap.
empty :: BiMap a b
empty = BiMap M.empty M.empty

-- | Create a BiMap with just one pair of values.
singleton :: a -> b -> BiMap a b
singleton va vb = BiMap (M.singleton va vb) (M.singleton vb va)

-- | Insert two values into the BiMap. Existing values will be replaced with the new values.
insert :: (Ord a, Ord b) => a -> b -> BiMap a b -> BiMap a b
insert va vb m = BiMap (M.insert va vb $ a2b m) (M.insert vb va $ b2a m)

-- | Convert a list of tuples into a BiMap.
fromList :: (Ord a, Ord b) =>  [(a, b)] -> BiMap a b
fromList = foldr (\(va, vb) r -> insert va vb r) empty

-- | Convert a map into a BiMap.
fromMap :: (Ord a, Ord b) => Map a b -> BiMap a b
fromMap = M.foldWithKey (\va vb r -> insert va vb r) empty