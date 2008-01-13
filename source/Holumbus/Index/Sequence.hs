-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Sequence
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Based upon the @DeepSeq@ class provided by HXT, this module exports 
  some more widely usable instances of @DeepSeq@ for maps and sets.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Sequence 
  (
    module Control.Strategies.DeepSeq
  )
where

import Control.Strategies.DeepSeq

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

instance (DeepSeq a) => DeepSeq (IntMap a) where
  deepSeq m y = IM.foldWithKey (\k v r -> deepSeq k $ deepSeq v r) y m
  
instance (DeepSeq a, DeepSeq b) => DeepSeq (Map a b) where  
  deepSeq m y = M.foldWithKey (\k v r -> deepSeq k $ deepSeq v r) y m
  
instance (DeepSeq a) => DeepSeq (StrMap a) where
  deepSeq m y = SM.foldWithKey (\k v r -> deepSeq k $ deepSeq v r) y m

instance DeepSeq IntSet where
  deepSeq s y = IS.fold deepSeq y s
