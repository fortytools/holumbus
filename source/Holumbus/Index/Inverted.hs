-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Inverted
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  The inverted index for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Inverted where

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.IntMap as IM

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Index.Common

data InvIndex    = InvIndex { docTable :: !Documents
                             , indexParts :: !Parts 
                             } deriving (Show)

type Parts       = Map Context Part    -- A context has a name and it's own index
type Part        = StrMap Occurrences  -- The word is the key with its occurrences as value

instance HolIndex InvIndex where
  empty = InvIndex emptyDocuments M.empty

  sizeDocs = IM.size . idToDoc . docTable
  sizeWords = M.fold ((+) . SM.size) 0 . indexParts
  documents = docTable
  contexts = map fst . M.toList . indexParts

  allWords c i = SM.toList $ getPart c i
  prefixCase c i q = SM.prefixFindNoCaseWithKey q $ getPart c i
  prefixNoCase c i q = SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase c i q = maybeToList (SM.lookup q $ getPart c i)
  lookupNoCase c i q = SM.lookupNoCase q $ getPart c i

  insert _ _ _ _ _ = empty -- TODO: This is just a dummy
  update _ _ _ _ _ = empty -- TODO: This is just a dummy

getPart :: Context -> InvIndex -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

