-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Control.Sequence
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

{-# OPTIONS -fno-warn-orphans #-}

module Holumbus.Control.Sequence 
  ( module Control.Strategies.DeepSeq )
where

import Control.Strategies.DeepSeq

import Data.Word

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

instance DeepSeq Word8
instance DeepSeq Word16
instance DeepSeq Word32
instance DeepSeq Word64

instance (DeepSeq a) => DeepSeq (IntMap a) where
  deepSeq m y = deepSeq (IM.toList m) y
  
instance (DeepSeq a, DeepSeq b) => DeepSeq (Map a b) where  
  deepSeq m y = deepSeq (M.toList m) y
  
instance (DeepSeq a) => DeepSeq (StrMap a) where
  deepSeq m y = deepSeq (SM.toList m) y

instance DeepSeq IntSet where
  deepSeq s y = deepSeq (IS.toList s) y
