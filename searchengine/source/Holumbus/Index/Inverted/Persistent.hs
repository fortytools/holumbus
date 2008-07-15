-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Persistent
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  A persistent version of the inverted index, which keeps the dictionary
  in memory but stores the Occurrences on disk. For extensive documentation 
  of the index interface, see class 'HolIndex' in "Holumbus.Index.Common".

  Implementation has been discontinued.

-}

-- ----------------------------------------------------------------------------
{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances -XMultiParamTypeClasses #-}

module Holumbus.Index.Inverted.Persistent 
(
  -- * Persistent index types
  Persistent (..)
, emptyPersistent  
--  , makePersistent
)
where

import System.IO.Unsafe
import Control.Exception()
import Control.Monad

import Control.Parallel.Strategies

import Data.List
import Data.Maybe
import Data.Binary hiding (Word)

import Data.Map (Map)
import qualified Data.Map as M

import Holumbus.Index.Common

import Holumbus.Control.MapReduce.MapReducible

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Utility

import Text.XML.HXT.Arrow.Pickle.Xml

-- | The index consists of a table which maps documents to ids and a number of index parts.
data Persistent = Persistent 
  { indexParts  :: Parts    -- ^ The parts of the index, each representing one context.
  , path        :: FilePath -- ^ Path to a directory containing occurences data
  , nextId      :: Int
  } deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to a file pointer.
type Part        = StrMap Int

instance MapReducible Persistent (Context, Word) Occurrences where
  mergeMR         = mergeIndexes
  reduceMR _ _ _  = undefined -- return $ Just $ foldl' (\i (w, d, p) -> insertPosition c w d p i) emptyInverted os 

instance HolIndex Persistent where
  sizeWords = M.fold ((+) . SM.size) 0 . indexParts
  contexts = map fst . M.toList . indexParts

  allWords     i c   = map (\(w, o) -> (w, retrieveOcc ((path i) ++ escape c ++ "_" ++ show o))) $ SM.toList                    $ getPart c i
  prefixCase   i c q = map (\(w, o) -> (w, retrieveOcc ((path i) ++ escape c ++ "_" ++ show o))) $ SM.prefixFindWithKey q       $ getPart c i
  prefixNoCase i c q = map (\(w, o) -> (w, retrieveOcc ((path i) ++ escape c ++ "_" ++ show o))) $ SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase   i c q = map (\o      -> (q, retrieveOcc ((path i) ++ escape c ++ "_" ++ show o))) $ maybeToList (SM.lookup q     $ getPart c i)
  lookupNoCase i c q = map (\(w, o) -> (w, retrieveOcc ((path i) ++ escape c ++ "_" ++ show o))) $ SM.lookupNoCase q            $ getPart c i

  mergeIndexes i1 i2   = foldl (\i (c,w,o) -> insertOccurrences c w o i) i1 (toList i2)
  
  substractIndexes _ _ = undefined

  {-# NOINLINE insertOccurrences #-}
  insertOccurrences c w o i 
    = unsafePerformIO
      ( do
        part <- return $ M.findWithDefault SM.empty c (indexParts i)
        if (not $ SM.member w part)
          then do -- occurrences file can not yet exist, so it can be written without merging
               writeToBinFile ((path i) ++ escape c ++ "_" ++ show (nextId i)) o -- show ( head $ unusedIds i)) o   
               return i { indexParts = M.insertWith (SM.union) c (SM.singleton w (nextId i) ) (indexParts i)
                        , nextId = (nextId i) + 1 --{unusedIds = tail $ unusedIds i}
                        }
          else do  -- the interesting case. occurences for the word already exist 
               ind <- SM.lookup w part
                 -- decoding has to be strict, else lazy io will lead to an exception
               occ <- strictDecodeFile ((path i) ++ escape c ++ "_" ++ show (ind))
               writeToBinFile ((path i) ++ escape c ++ "_" ++ show (ind)) (mergeOccurrences o occ)
               return i
      ) 

  deleteOccurrences _ _ _ _ = undefined

  splitByContexts _ = undefined
  splitByDocuments _ = undefined
  splitByWords _ = undefined

  updateDocIds _ _ = undefined
  
  toList i = concat $ map convertPart $ M.toList (indexParts i) 
    where convertPart (c,p) = map (\(w, o) -> (c, w, retrieveOcc $ (path i) ++ escape c ++ "_" ++ show o)) $ SM.toList $ p

instance Binary Persistent where
  put (Persistent parts thePath _) = put parts >> put thePath -- >> put i
  get = do
        parts   <- get
        thePath <- get 
        return $ Persistent parts thePath (maximum $ concat $ M.elems $ M.map SM.elems parts)

instance XmlPickler Persistent where
  xpickle = xpZero

instance NFData Persistent where
  rnf (Persistent parts _ _) = rnf parts

emptyPersistent :: FilePath -> Persistent
emptyPersistent f = Persistent M.empty f 1 --[1..]

-- | Return a part of the index for a given context.
getPart :: Context -> Persistent -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- | Read occurrences from disk through unsafe IO.
retrieveOcc :: FilePath -> Occurrences
retrieveOcc f = unsafePerformIO (strictDecodeFile f)

