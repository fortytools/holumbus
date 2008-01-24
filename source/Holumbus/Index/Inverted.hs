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
import Data.Binary

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Data.DiffList (DiffList)
import qualified Holumbus.Data.DiffList as DL

import Holumbus.Index.Common
import Holumbus.Index.Documents

import Control.Parallel.Strategies

-- | The index consists of a table which maps documents to ids and a number of index parts.
data InvIndex    = InvIndex { docTable :: !Documents
                            , indexParts :: !Parts 
                            } deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to their occurrences.
type Part        = StrMap (IntMap DiffList)

instance HolIndex InvIndex where
  sizeDocs = IM.size . idToDoc . docTable
  sizeWords = M.fold ((+) . SM.size) 0 . indexParts
  documents = docTable
  contexts = map fst . M.toList . indexParts

  allWords c i = map (\(w, o) -> (w, inflate o)) $ SM.toList $ getPart c i
  prefixCase c i q = map (\(w, o) -> (w, inflate o)) $ SM.prefixFindWithKey q $ getPart c i
  prefixNoCase c i q = map (\(w, o) -> (w, inflate o)) $ SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase c i q = map inflate $ maybeToList (SM.lookup q $ getPart c i)
  lookupNoCase c i q = map inflate $ SM.lookupNoCase q $ getPart c i

  insert _ _ _ _ _ = empty -- TODO: This is just a dummy
  update _ _ _ _ _ = empty -- TODO: This is just a dummy

instance NFData InvIndex where
  rnf (InvIndex docs parts) = rnf docs `seq` rnf parts

instance XmlPickler InvIndex where
  xpickle =  xpWrap (\(dt, ip) -> InvIndex dt ip, \(InvIndex dt ip) -> (dt, ip))
             (xpPair xpDocuments xpParts)

instance Binary InvIndex where
  put (InvIndex docs parts) = do
                              put docs
                              put parts
  get = do
        docs <- get
        parts <- get
        return (InvIndex docs parts)

-- | Convert the differences back to a set of integers.
inflate :: IntMap DiffList -> Occurrences
inflate = IM.map DL.toIntSet

-- | Save some memory on the positions by just saving their differences.
deflate :: Occurrences -> IntMap DiffList
deflate = IM.map DL.fromIntSet

-- | Create an empty index.
empty :: InvIndex
empty = InvIndex emptyDocuments M.empty

-- | Load index from XML file.
loadFromXmlFile :: FilePath -> IO InvIndex
loadFromXmlFile f = do
                    r <- runX (xunpickleDocument xpInvIndex options f)
                    return $ head r
                    where
                    options = [ (a_remove_whitespace, v_1), (a_validate, v_0) ]			

-- | Write index to XML file.
writeToXmlFile :: FilePath -> InvIndex -> IO ()
writeToXmlFile f i = do
                     runX (constA i >>> xpickleDocument xpInvIndex options f)
                     return ()
                     where
                     options = [ (a_indent, v_1), (a_validate, v_0), (a_output_encoding, utf8) ]     

-- | Load index from a binary file.
loadFromBinFile :: FilePath -> IO InvIndex
loadFromBinFile f = decodeFile f

-- | Write index to a binary file.
writeToBinFile :: FilePath -> InvIndex -> IO ()
writeToBinFile =  encodeFile
                  
-- | Return a part of the index for a given context.
getPart :: Context -> InvIndex -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- | The XML pickler for an inverted index.
xpInvIndex :: PU InvIndex
xpInvIndex = xpElem "indexes" $ xpickle

-- | The XML pickler for the index parts.
xpParts :: PU Parts
xpParts = xpWrap (M.fromList, M.toList) (xpList xpContext)
  where
  xpContext = xpElem "part" (xpPair (xpAttr "id" xpText) xpPart)

-- | The XML pickler for a single part.
xpPart :: PU Part
xpPart = xpElem "index" (xpWrap (SM.fromList, SM.toList) (xpList xpWord))
  where
  xpWord = xpElem "word" (xpPair (xpAttr "w" xpText) (xpWrap (deflate, inflate) xpOccurrences))
