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

import Text.XML.HXT.Arrow

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.IntMap as IM

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Index.Common
import Holumbus.Index.Documents
import Holumbus.Control.Sequence

-- | The index consists of a table which maps documents to ids and a number of index parts.
data InvIndex    = InvIndex { docTable :: !Documents
                             , indexParts :: !Parts 
                             } deriving (Show)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to their occurrences.
type Part        = StrMap Occurrences

instance HolIndex InvIndex where
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

instance DeepSeq InvIndex where
  deepSeq (InvIndex docs parts) b = deepSeq docs $ deepSeq parts b

-- | Create an empty index.
empty :: InvIndex
empty = InvIndex emptyDocuments M.empty

-- | Load Index from XML file
loadFromFile :: String -> IO InvIndex
loadFromFile f = do
                 r <- runX (xunpickleDocument xpInvIndex options f)
                 return $ strict (head r)
                 where
                 options = [ (a_remove_whitespace, v_1), (a_validate, v_0) ]			

-- | Return a part of the index for a given context.
getPart :: Context -> InvIndex -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- | The XML pickler for an inverted index.
xpInvIndex :: PU InvIndex
xpInvIndex = xpElem "indexes" $ xpickle

instance XmlPickler InvIndex where
  xpickle =  xpWrap (\(dt, ip) -> InvIndex dt ip, \(InvIndex dt ip) -> (dt, ip))
             (xpPair xpDocuments xpParts)

-- | The XML pickler for the index parts.
xpParts :: PU Parts
xpParts = xpWrap (M.fromList, M.toList) (xpList xpContext)
  where
  xpContext = xpElem "part" (xpPair (xpAttr "id" xpText) xpPart)

-- | The XML pickler for a single part.
xpPart :: PU Part
xpPart = xpElem "index" (xpWrap (SM.fromList, SM.toList) (xpList xpWord))
  where
  xpWord = xpElem "word" (xpPair (xpAttr "w" xpText) xpOccurrences)
