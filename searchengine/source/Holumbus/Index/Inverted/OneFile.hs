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

module Holumbus.Index.Inverted.OneFile 
(
  -- * Persistent index types
  Persistent (..)
  , makePersistent
)
where

import System.IO
import System.IO.Unsafe
import Control.Exception
import Control.Monad

import Control.Parallel.Strategies

import Data.Maybe
import Data.Binary

import qualified Data.ByteString.Lazy as B

import Data.Map (Map)
import qualified Data.Map as M

import Holumbus.Index.Common
import Holumbus.Index.Compression

import Holumbus.Index.Inverted.Memory (Inverted (Inverted))

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

-- | The index consists of a table which maps documents to ids and a number of index parts.
data Persistent = Persistent 
  { indexParts :: Parts     -- ^ The parts of the index, each representing one context.
  , occurrences :: FilePath -- ^ Path to a file containing temorary data.
  } deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to a file pointer.
type Part        = StrMap (Integer, Int)

instance HolIndex Persistent where
  sizeWords = M.fold ((+) . SM.size) 0 . indexParts
  contexts = map fst . M.toList . indexParts

  allWords i c = map (\(w, o) -> (w, retrieveOcc (occurrences i) o)) $ SM.toList $ getPart c i
  prefixCase i c q = map (\(w, o) -> (w, retrieveOcc (occurrences i) o)) $ SM.prefixFindWithKey q $ getPart c i
  prefixNoCase i c q = map (\(w, o) -> (w, retrieveOcc (occurrences i) o)) $ SM.prefixFindNoCaseWithKey q $ getPart c i
  lookupCase i c q = map (\o -> (q, retrieveOcc (occurrences i) o)) $ maybeToList (SM.lookup q $ getPart c i)
  lookupNoCase i c q = map (\(w, o) -> (w, retrieveOcc (occurrences i) o)) $ SM.lookupNoCase q $ getPart c i

  mergeIndexes _ _ = undefined
  substractIndexes _ _ = undefined

  insertOccurrences _ _ _ _ = undefined
  deleteOccurrences _ _ _ _ = undefined

  splitByContexts _ = undefined
  splitByDocuments _ = undefined
  splitByWords _ = undefined

  updateDocIds _ _ = undefined
  
  toList _ = undefined

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

-- | Return a part of the index for a given context.
getPart :: Context -> Persistent -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- | Read occurrences from disk through unsafe IO.
retrieveOcc :: FilePath -> (Integer, Int) -> Occurrences
retrieveOcc f o = unsafePerformIO (handle (\_ -> return emptyOccurrences) (readOccurrences o f))

-- | Write occurrences to disk through unsafe IO.
storeOcc :: FilePath -> Occurrences -> (Integer, Int)
storeOcc f o = unsafePerformIO (handle (\_ -> return (0,0)) (writeOccurrences o f))

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
