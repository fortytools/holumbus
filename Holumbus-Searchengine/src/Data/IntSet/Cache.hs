module Data.IntSet.Cache
where

import           Control.DeepSeq

import qualified Data.IntSet     as S
import qualified Data.Vector     as V

-- ------------------------------------------------------------
--
-- cache for single element IntSets with elements 0 <= i < cacheSize

cacheSize :: Int
cacheSize = 1024

cache :: V.Vector S.IntSet
cache = id $!! V.generate cacheSize S.singleton

cacheAt :: Int -> S.IntSet
cacheAt i
    | 0 <= i
      &&
      i < cacheSize
          = id $! cache V.! i
    | otherwise
        = id $! S.singleton i

s0 :: S.IntSet
s0 = cacheAt 0

s1 :: S.IntSet
s1 = cacheAt 1

-- ------------------------------------------------------------



