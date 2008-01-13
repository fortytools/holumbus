-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Combined
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Combines all functions on indexes for every index type in just one
  single data type.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Combined where

import Holumbus.Index.Common

-- Import all supported index types.
import Holumbus.Index.Inverted
import Holumbus.Index.Hybrid

-- Imports for providing instances of DeepSeq
import Control.Strategies.DeepSeq
import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- | This is the combined index type, which combines all functions required by @HolIndex@
-- for every index type.
data AnyIndex = Inv InvIndex
              | Hyb HybIndex

instance HolIndex AnyIndex where
  sizeDocs (Inv i) = sizeDocs i
  sizeDocs (Hyb i) = sizeDocs i

  sizeWords (Inv i) = sizeWords i
  sizeWords (Hyb i) = sizeWords i
  
  documents (Inv i) = documents i
  documents (Hyb i) = documents i

  contexts (Inv i) = contexts i
  contexts (Hyb i) = contexts i

  allWords c (Inv i) = allWords c i
  allWords c (Hyb i) = allWords c i

  prefixCase c (Inv i) s = prefixCase c i s
  prefixCase c (Hyb i) s = prefixCase c i s

  prefixNoCase c (Inv i) s = prefixNoCase c i s
  prefixNoCase c (Hyb i) s = prefixNoCase c i s

  lookupCase c (Inv i) s = lookupCase c i s
  lookupCase c (Hyb i) s = lookupCase c i s

  lookupNoCase c (Inv i) s = lookupNoCase c i s
  lookupNoCase c (Hyb i) s = lookupNoCase c i s

  insert c w p d (Inv i) = Inv (insert c w p d i)
  insert c w p d (Hyb i) = Hyb (insert c w p d i)

  update c w p d (Inv i) = Inv (update c w p d i)
  update c w p d (Hyb i) = Hyb (update c w p d i)

instance DeepSeq InvIndex where
  deepSeq (InvIndex docs parts) b = deepSeq parts b

--instance DeepSeq Documents where
--  deepSeq (Documents i2d d2i lid) b = deepSeq i2d $ deepSeq d2i $ deepSeq lid b

instance (DeepSeq a) => DeepSeq (IntMap a) where
  deepSeq m y = IM.foldWithKey (\k v r -> deepSeq k $ deepSeq v r) y m
  
instance (DeepSeq a, DeepSeq b) => DeepSeq (Map a b) where  
  deepSeq m y = M.foldWithKey (\k v r -> deepSeq k $ deepSeq v r) y m
  
instance (DeepSeq a) => DeepSeq (StrMap a) where
  deepSeq m y = SM.foldWithKey (\k v r -> deepSeq k $ deepSeq v r) y m

instance DeepSeq IntSet where
  deepSeq s y = IS.fold deepSeq y s
