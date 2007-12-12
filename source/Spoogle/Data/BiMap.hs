-- ----------------------------------------------------------------------------

{- |
  Module     : Spoogle.Data.BiMap
  Copyright  : Copyright (C) 2007 Timo B. Hübel
  License    : MIT

  Maintainer : Timo B. Hübel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : $Id$

  A bijective map based on the Haskell default map.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Data.BiMap
  (
    -- * BiMap type
    BiMap
    
    -- * Query
    , lookupA
    , lookupB
    
    -- * Construction
    , insert
  )
where

import Data.Map (Map)
import qualified Data.Map as M

data (Ord a, Ord b) => BiMap a b = BM { a2b :: !(Map a b), b2a :: !(Map b a) } deriving (Show)

lookupA :: (Ord a, Ord b) => a -> BiMap a b -> Maybe b
lookupA k m = M.lookup k (a2b m)

lookupB :: (Ord a, Ord b) => b -> BiMap a b -> Maybe a
lookupB k m = M.lookup k (b2a m)

insert :: (Ord a, Ord b) => a -> b -> BiMap a b -> BiMap a b
insert va vb m = BM (M.insert va vb $ a2b m) (M.insert vb va $ b2a m)

