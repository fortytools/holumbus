-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Hybrid
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The hybrid index for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Hybrid where

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Index.Common
import Holumbus.Index.Documents
import Holumbus.Index.Sequence

import Text.XML.HXT.Arrow   			-- Import stuff for pickling

data HybIndex      = HybIndex { docTable   :: !Documents
                              , indexParts :: !Parts 
                              } deriving (Show)

type Parts         = Map Context Part
data Part          = Part { dictionary :: !Dictionary
                          , blocks     :: !Blocks 
                          } deriving (Show)

data Dictionary    = Dictionary { wordTable  :: !(StrMap WordInfo)
                                , lastWordId :: !WordId
                                } deriving (Show)

type WordInfo      = ( WordId, BlockId )

data Blocks        = Blocks { blockTable  :: !(IntMap Block)
                            , lastBlockId :: !BlockId
                            } deriving (Show)

type Block         = [ Occurrence ]               -- Sorted by DocId
type Occurrence    = ( DocId, WordId, Position )

type WordId        = Int
type BlockId       = Int

instance HolIndex HybIndex where
  sizeDocs _ = 0
  sizeWords _ = 0
  documents = docTable
  contexts = map fst . M.toList . indexParts

  allWords _ _ = [] -- TODO: This is just a dummy
  prefixCase _ _ _ = [] -- TODO: This is just a dummy
  prefixNoCase _ _ _ = [] -- TODO: This is just a dummy
  lookupCase _ _ _ = [] -- TODO: This is just a dummy
  lookupNoCase _ _ _ = [] -- TODO: This is just a dummy

  insert _ _ _ _ _ = empty -- TODO: This is just a dummy
  update _ _ _ _ _ = empty -- TODO: This is just a dummy

instance DeepSeq HybIndex where
  deepSeq (HybIndex docs parts) b = deepSeq docs $ deepSeq parts b

instance DeepSeq Part where
  deepSeq (Part dic blocks) b = deepSeq dic $ deepSeq blocks b

instance DeepSeq Dictionary where
  deepSeq (Dictionary tab lid) b = deepSeq tab $ deepSeq lid b

instance DeepSeq Blocks where
  deepSeq (Blocks tab lid) b = deepSeq tab $ deepSeq lid b

-- | Create an empty index.
empty :: HybIndex
empty = HybIndex emptyDocuments M.empty

-- | Load Index from XML file
loadFromFile :: String -> IO HybIndex
loadFromFile f = do
                 r <- runX (xunpickleDocument xpHybIndex options f)
                 return $ strict (head r)
                 where
                 options = [ (a_remove_whitespace, v_1), (a_validate, v_0) ]

-- | Create an empty part.
emptyPart :: Part
emptyPart = Part emptyDictionary emptyBlocks

-- | Create an empty dictionary.
emptyDictionary :: Dictionary
emptyDictionary = Dictionary SM.empty 0

-- | Create empty blocks.
emptyBlocks :: Blocks
emptyBlocks = Blocks IM.empty 0

-- | Return a part of the index for a given context.
getPart :: Context -> HybIndex -> Part
getPart c i = fromMaybe emptyPart (M.lookup c $ indexParts i)

-- | The pickler for an hybrid index.
xpHybIndex :: PU HybIndex
xpHybIndex = xpElem "indexes" $
	    xpickle

instance XmlPickler HybIndex where
    xpickle =  xpZero