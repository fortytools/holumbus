-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Hybrid
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The hybrid index for Holumbus.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Hybrid where

import Data.Maybe
import Data.Binary

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Index.Common

import Text.XML.HXT.Arrow   			-- Import stuff for pickling

newtype HybIndex = HybIndex { indexParts :: Parts } deriving (Show, Eq)

type Parts         = Map Context Part
data Part          = Part { dictionary :: !Dictionary
                          , blocks     :: !Blocks 
                          } deriving (Show, Eq)

data Dictionary    = Dictionary { wordTable  :: !(StrMap WordInfo)
                                , lastWordId :: !WordId
                                } deriving (Show, Eq)

type WordInfo      = ( WordId, BlockId )

data Blocks        = Blocks { blockTable  :: !(IntMap Block)
                            , lastBlockId :: !BlockId
                            } deriving (Show, Eq)

type Block         = [ Occurrence ]               -- Sorted by DocId
type Occurrence    = ( DocId, WordId, Position )

type WordId        = Int
type BlockId       = Int

instance HolIndex HybIndex where
  sizeWords _ = undefined
  contexts = map fst . M.toList . indexParts

  allWords _ _ = undefined -- TODO: This is just a dummy
  prefixCase _ _ _ = undefined -- TODO: This is just a dummy
  prefixNoCase _ _ _ = undefined -- TODO: This is just a dummy
  lookupCase _ _ _ = undefined -- TODO: This is just a dummy
  lookupNoCase _ _ _ = undefined -- TODO: This is just a dummy

  mergeIndexes _ _ = undefined
  substractIndexes _ _ = undefined

  insertOccurrences _ _ _ _ = undefined
  deleteOccurrences _ _ _ _ = undefined
  
  splitByContexts _ _ = undefined
  splitByDocuments _ _ = undefined
  splitByWords _ _ = undefined
  
  updateDocuments _ _ = undefined

instance Binary HybIndex where
  put _ = undefined
  get = undefined

-- | Create an empty index.
emptyHybrid :: HybIndex
emptyHybrid = HybIndex M.empty

-- | Load Index from XML file
loadFromXmlFile :: String -> IO HybIndex
loadFromXmlFile f = do
                    r <- runX (xunpickleDocument xpHybIndex options f)
                    return $ head r
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