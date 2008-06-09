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

module Holumbus.Index.Inverted.Persistent 
(
  -- * Persistent index types
  Persistent (..)
, emptyPersistent  
--  , makePersistent
)
where

import System.IO
import System.IO.Unsafe
import Control.Exception()
import Control.Monad

import Control.Parallel.Strategies

import Data.Maybe
import Data.Binary
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.Lazy as B

import Data.Map (Map)
import qualified Data.Map as M

import Holumbus.Index.Common
-- import Holumbus.Index.Compression

-- import Holumbus.Index.Inverted (Inverted (Inverted))

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Holumbus.Utility

-- | The index consists of a table which maps documents to ids and a number of index parts.
data Persistent = Persistent 
  { indexParts  :: Parts    -- ^ The parts of the index, each representing one context.
  , path        :: FilePath -- ^ Path to a directory containing occurences data
  , nextId      :: Int
--  , unusedIds   :: [Int]    -- ^ Unused IDs for file  naming
  } deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to a file pointer.
type Part        = StrMap Int

{-

let p = fromList (emptyPersistent "/home/sms/tmp/persistent/") (toList (insertPosition "c" "foo" 1 42 (insertPosition "c" "bar" 1 43 (insertPosition "d" "batz" 1 77 emptyInverted ))))


:m + Holumbus.Index.Common Holumbus.Index.Inverted Holumbus.Index.Inverted.Persistent


-}

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

instance NFData Persistent where
  rnf (Persistent parts _ _) = rnf parts

emptyPersistent :: FilePath -> Persistent
emptyPersistent f = Persistent M.empty f 1 --[1..]


-- | Create a persistent index from an inverted index.
{-
makePersistent :: FilePath -> Inverted -> Persistent
makePersistent f (Inverted parts) = Persistent (store parts) f
  where
  store = M.map (SM.map (storeOcc f . inflateOcc))
-}


-- | Return a part of the index for a given context.
getPart :: Context -> Persistent -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

-- | Read occurrences from disk through unsafe IO.
retrieveOcc :: FilePath -> Occurrences
retrieveOcc f = unsafePerformIO (loadFromBinFile f)
 
-- found this on the haskell cafe mailing list
-- http://www.haskell.org/pipermail/haskell-cafe/2008-April/041970.html
strictDecodeFile :: Binary a => FilePath -> IO a
strictDecodeFile path  =
    bracket (openBinaryFile path ReadMode) hClose $ \h -> do
      c <- B.hGetContents h
      return $! decode c  
