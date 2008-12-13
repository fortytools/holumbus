-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Persistent
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  A persistent version of the inverted index, which keeps the dictionary
  in memory but stores the Occurrences on disk. For extensive documentation 
  of the index interface, see class 'HolIndex' in "Holumbus.Index.Common".

-}

-- ----------------------------------------------------------------------------
{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances -XMultiParamTypeClasses #-}

module Holumbus.Index.Inverted.OneFile 

(
  -- * Persistent index types
  Persistent (..)
, makePersistent
, emptyPersistent
)
where

import System.IO
import System.IO.Unsafe
import Control.Exception
import Control.Monad

import Control.Parallel.Strategies

import Data.Maybe
import Data.Binary hiding (Word)

import qualified Data.ByteString.Lazy as B

import Data.Map (Map)
import qualified Data.Map as M

import Holumbus.Control.MapReduce.MapReducible

import Holumbus.Index.Common
import Holumbus.Index.Compression

import Holumbus.Index.Inverted.Memory (Inverted (Inverted))

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Text.XML.HXT.Arrow.Pickle

-- | The index consists of a table which maps documents to ids and a number of index parts.
data Persistent = Persistent 
  { indexParts :: Parts     -- ^ The parts of the index, each representing one context.
  , occurrences :: FilePath -- ^ Path to a file containing temorary data.
  } deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to a file pointer.
type Part        = StrMap (Integer, Int)

instance XmlPickler Persistent where
  xpickle =  xpZero

instance MapReducible Persistent (Context, Word) Occurrences where
  reduceMR = undefined
  mergeMR = mergeIndexesM

instance HolIndex Persistent where
  sizeWords = M.fold ((+) . SM.size) 0 . indexParts
  contexts  = map fst . M.toList . indexParts

  allWords i c       = map (rawHelper (occurrences i)) $ SM.toList $ getPart c i
  prefixCase i c q   = map (rawHelper (occurrences i)) $ SM.prefixFindWithKey q $ getPart c i
  prefixNoCase i c q = map (rawHelper (occurrences i)) $ SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase i c q   = map (rawHelper (occurrences i)) $ zip (repeat q) (maybeToList (SM.lookup q $ getPart c i))
  lookupNoCase i c q = map (rawHelper (occurrences i)) $ SM.lookupNoCase q $ getPart c i

  mergeIndexes _ _ = undefined
  substractIndexes = undefined

  insertOccurrences c w o i = unsafePerformIO (insertOccurrencesM c w o i)
  deleteOccurrences _ _ _ _ = undefined

  splitByContexts = undefined
  splitByDocuments = undefined
  splitByWords = undefined

  updateDocIds _ _ = undefined
  
  toList _ = undefined
  
instance HolIndexM IO Persistent where
  sizeWordsM = return . M.fold ((+) . SM.size) 0 . indexParts
  contextsM  = return . map fst . M.toList . indexParts

  allWordsM i c       = mapM (rawHelperM (occurrences i)) $ SM.toList $ getPart c i
  prefixCaseM i c q   = mapM (rawHelperM (occurrences i)) $ SM.prefixFindWithKey q $ getPart c i
  prefixNoCaseM i c q = mapM (rawHelperM (occurrences i)) $ SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCaseM i c q   = mapM (rawHelperM (occurrences i)) $ zip (repeat q) (maybeToList (SM.lookup q $ getPart c i))
  lookupNoCaseM i c q = mapM (rawHelperM (occurrences i)) $ SM.lookupNoCase q $ getPart c i

  mergeIndexesM _ _ = undefined

  insertOccurrencesM c w o i 
    = let part = M.findWithDefault SM.empty c (indexParts i) in
      if (SM.member w part)
        then error "Holumbus.Index.Inverted.OneFile: Inserting additional occurrences not supported"
        else do
             op <- return $ storeOcc (occurrences i) o
             return i {indexParts = M.insertWith (SM.union) c (SM.singleton w op) (indexParts i)}
                                                    
                               
  deleteOccurrencesM _ _ _ _ = undefined

  updateDocIdsM _ _ = undefined
  
  toListM _ = undefined
  

instance Binary Persistent where
  put (Persistent parts occ) = put parts >> put occ
  get = liftM2 Persistent get get

instance NFData Persistent where
  rnf (Persistent parts occ) = rnf parts `seq` rnf occ

-- | Create a persistent index from an inverted index.
makePersistent :: FilePath -> Inverted -> Persistent
makePersistent f (Inverted parts) = Persistent (store parts) f
  where
  store = M.map (SM.map (storeOcc f . inflateOcc))

emptyPersistent :: FilePath -> Persistent
emptyPersistent = Persistent M.empty

-- | Return a part of the index for a given context.
getPart :: Context -> Persistent -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)


rawHelper :: FilePath -> (Word, (Integer, Int)) -> (Word, Occurrences)
rawHelper f (w,o) = unsafePerformIO (rawHelperM f (w,o))

rawHelperM :: FilePath -> (Word, (Integer, Int)) -> IO (Word, Occurrences)
rawHelperM f (w,o) = do; occ <- retrieveOcc f o; return (w, occ)

-- | Read occurrences from disk through unsafe IO.
retrieveOcc :: FilePath -> (Integer, Int) -> IO Occurrences
retrieveOcc f o = handle handler (readOccurrences o f)
		  where
		  handler :: SomeException -> IO Occurrences
		  handler = const $ return emptyOccurrences

-- | Write occurrences to disk through unsafe IO.
storeOcc :: FilePath -> Occurrences -> (Integer, Int)
storeOcc f o = unsafePerformIO $ handle handler (writeOccurrences o f)
	       where
	       handler :: SomeException -> IO (Integer, Int)
	       handler = const $ return (0,0)
	       
readOccurrences :: (Integer, Int) -> FilePath -> IO Occurrences
readOccurrences (pos, len) f = 
  do
  h <- openBinaryFile f ReadMode
  hSeek h AbsoluteSeek pos
  r <- B.hGet h len
  hClose h
  return $! decode r

writeOccurrences :: Occurrences -> FilePath -> IO (Integer, Int)
writeOccurrences o f =
  do
  h <- openBinaryFile f ReadWriteMode
  hSeek h SeekFromEnd 0
  pos <- hTell h
  r <- return $! encode o
  B.hPut h r
  hClose h
  return (pos, fromIntegral $ B.length r)

