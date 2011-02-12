{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances -XMultiParamTypeClasses #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Persistent
  Copyright  : Copyright (C) 2008 - 2009 Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  
  A persistent version of the inverted index, which keeps the dictionary
  in memory but stores the Occurrences on disk. For extensive documentation 
  of the index interface, see class 'HolIndex' in "Holumbus.Index.Common".

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Inverted.OneFile 
    (
     -- * Persistent index types
      Persistent (..)
    , makePersistent
    , emptyPersistent
    )
where

import           Control.Exception
import           Control.Monad
import           Control.DeepSeq

import           Data.Binary            hiding (Word)
import qualified Data.ByteString.Lazy   as B
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe



import           Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM
import           Holumbus.Index.Common
import           Holumbus.Index.Compression
import           Holumbus.Index.Inverted.Memory         (Inverted (Inverted))

import           System.IO
import           System.IO.Unsafe

import           Text.XML.HXT.Arrow.Pickle

-- ----------------------------------------------------------------------------

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts              = Map Context Part

-- | The index part is the real inverted index. Words are mapped to a file pointer.
type Part               = StrMap (Integer, Int)

-- ----------------------------------------------------------------------------

-- | The index consists of a table which maps documents to ids and a number of index parts.
data Persistent         = Persistent
                          { indexParts :: Parts     -- ^ The parts of the index, each representing one context.
                          , occurrences :: FilePath -- ^ Path to a file containing temorary data.
                          } deriving (Show, Eq)

instance HolIndexM IO Persistent where
  sizeWordsM            = return . M.fold ((+) . SM.size) 0 . indexParts
  contextsM             = return . map fst . M.toList . indexParts

  allWordsM i c         = mapM (rawHelperM (occurrences i)) $ SM.toList $ getPart c i
  prefixCaseM i c q     = mapM (rawHelperM (occurrences i)) $ SM.prefixFindWithKey q $ getPart c i
  prefixNoCaseM i c q   = mapM (rawHelperM (occurrences i)) $ SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCaseM i c q     = mapM (rawHelperM (occurrences i)) $ zip (repeat q) (maybeToList (SM.lookup q $ getPart c i))
  lookupNoCaseM i c q   = mapM (rawHelperM (occurrences i)) $ SM.lookupNoCase q $ getPart c i

  toListM               = return . toList

  insertOccurrencesM    = insertOccurrencesM'
                                                    
  mergeIndexesM _ _             = error "Holumbus.Index.Inverted.OneFile: mergeIndexesM not supported"
  deleteOccurrencesM _ _ _ _    = error "Holumbus.Index.Inverted.OneFile: deleteOccurrencesM not supported"
  updateDocIdsM _ _             = error "Holumbus.Index.Inverted.OneFile: updateDocIdsM not supported"
  updateDocIdsM' _ _            = error "Holumbus.Index.Inverted.OneFile: updateDocIdsM' not supported"

-- Required to avoid overlapping instances
insertOccurrencesM' :: (Monad m) => Context -> Word -> Occurrences -> Persistent -> m Persistent
insertOccurrencesM' c w o i 
                        = let
                          part = M.findWithDefault SM.empty c (indexParts i)
                          in
                          if (SM.member w part)
                             then error "Holumbus.Index.Inverted.OneFile: Inserting additional occurrences not supported"
                             else do
                                  op <- return $ storeOcc (occurrences i) o
                                  return i {indexParts = M.insertWith (SM.union) c (SM.singleton w op) (indexParts i)}


-- This is a little bit dirty due to the use of unsafePerformIO, but Hayoo! currently uses this instance...
instance HolIndex Persistent where 
  sizeWords                 = M.fold ((+) . SM.size) 0 . indexParts 
  contexts                  = map fst . M.toList . indexParts 

  allWords i c              = map (rawHelper (occurrences i)) $ SM.toList $ getPart c i 
  prefixCase i c q          = map (rawHelper (occurrences i)) $ SM.prefixFindWithKey q $ getPart c i 
  prefixNoCase i c q        = map (rawHelper (occurrences i)) $ SM.prefixFindNoCaseWithKey q $ getPart c i 
  lookupCase i c q          = map (rawHelper (occurrences i)) $ zip (repeat q) (maybeToList (SM.lookup q $ getPart c i)) 
  lookupNoCase i c q        = map (rawHelper (occurrences i)) $ SM.lookupNoCase q $ getPart c i 

  toList i                  = [ (c,w,o) | c     <- M.keys . indexParts $ i
                                        , (w,o) <- allWords i c
                              ]
  
  mergeIndexes _ _          = error "Holumbus.Index.Inverted.OneFile: mergeIndexes not supported" 
  substractIndexes          = error "Holumbus.Index.Inverted.OneFile: substractIndexes not supported" 

  insertOccurrences c w o i = unsafePerformIO (insertOccurrencesM' c w o i) 
  deleteOccurrences _ _ _ _ = error "Holumbus.Index.Inverted.OneFile: deleteOccurrences not supported" 

  splitByContexts           = error "Holumbus.Index.Inverted.OneFile: splitByContexts not supported" 
  splitByDocuments          = error "Holumbus.Index.Inverted.OneFile: splitByDocuments not supported" 
  splitByWords              = error "Holumbus.Index.Inverted.OneFile: splitByWords not supported"  

  updateDocIds _ _          = error "Holumbus.Index.Inverted.OneFile: updateDocIds not supported" 

instance XmlPickler Persistent where
  xpickle               =  xpZero               -- DUMMY

instance Binary Persistent where
  put (Persistent parts occ)
                        = put parts >> put occ
  get                   = liftM2 Persistent get get

instance NFData Persistent where
  rnf (Persistent parts occ)
                         = rnf parts `seq` rnf occ

-- ----------------------------------------------------------------------------

-- | Create a persistent index from an inverted index.
makePersistent          :: FilePath -> Inverted -> Persistent
makePersistent f (Inverted parts)
                         = Persistent (store parts) f
    where
    store               = M.map (SM.map (storeOcc f . inflateOcc))

emptyPersistent         :: FilePath -> Persistent
emptyPersistent         = Persistent M.empty

-- | Return a part of the index for a given context.
getPart                 :: Context -> Persistent -> Part
getPart c i             = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- Read the note about the HolIndex instance above...
rawHelper :: FilePath -> (Word, (Integer, Int)) -> (Word, Occurrences) 
rawHelper f (w,o) = unsafePerformIO (rawHelperM f (w,o)) 

rawHelperM              :: FilePath -> (Word, (Integer, Int)) -> IO (Word, Occurrences)
rawHelperM f (w,o)      = do
                          occ <- retrieveOcc f o
                          return (w, occ)

-- | Read occurrences from disk through unsafe IO.
retrieveOcc             :: FilePath -> (Integer, Int) -> IO Occurrences
retrieveOcc f o         = handle handler (readOccurrences o f)
    where
    handler             :: SomeException -> IO Occurrences
    handler             = const $ return emptyOccurrences

-- | Write occurrences to disk through unsafe IO.
storeOcc                :: FilePath -> Occurrences -> (Integer, Int)
storeOcc f o            = unsafePerformIO $ handle handler (writeOccurrences o f)
    where
    handler             :: SomeException -> IO (Integer, Int)
    handler             = const $ return (0,0)
               
readOccurrences         :: (Integer, Int) -> FilePath -> IO Occurrences
readOccurrences (pos, len) f
                        = do
                          h <- openBinaryFile f ReadMode
                          hSeek h AbsoluteSeek pos
                          r <- B.hGet h len
                          hClose h
                          return $! decode r

writeOccurrences        :: Occurrences -> FilePath -> IO (Integer, Int)
writeOccurrences o f    = do
                          h <- openBinaryFile f ReadWriteMode
                          hSeek h SeekFromEnd 0
                          pos <- hTell h
                          r <- return $! encode o
                          B.hPut h r
                          hClose h
                          return (pos, fromIntegral $ B.length r)

-- ----------------------------------------------------------------------------
