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
data (Ord a, Ord b) => BiMap a b = BiMap { a2b :: !(Map a b)
                                         , b2a :: !(Map b a) 
                                         } deriving (Show)

-- | Lookup the value b for another value a.
lookupA :: (Ord a, Ord b) => a -> BiMap a b -> Maybe b
lookupA k m = M.lookup k (a2b m)

-- | Lookup the value a for another value b.
lookupB :: (Ord a, Ord b) => b -> BiMap a b -> Maybe a
lookupB k m = M.lookup k (b2a m)

-- | Create an empty BiMap.
empty :: (Ord a, Ord b) => BiMap a b
empty = BiMap M.empty M.empty

-- | Create a BiMap with just one pair of values.
singleton :: (Ord a, Ord b) => a -> b -> BiMap a b
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