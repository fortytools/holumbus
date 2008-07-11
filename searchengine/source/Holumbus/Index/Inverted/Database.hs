{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances -XMultiParamTypeClasses #-}
-- ----------------------------------------------------------------------------


{- |
  Module     : Holumbus.Index.Inverted.Database
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Timo B. Huebel (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  A persistent version of the inverted index, which keeps the dictionary
  in memory but stores the Occurrences on disk. To avoid problems with data
  fragmentation, SQLite is used as data backend. For extensive documentation 
  of the index interface, see class 'HolIndex' in "Holumbus.Index.Common".

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Inverted.Database 
{- (
  -- * Persistent index types
  InvertedWithDatabase (..)
, emptyInvertedWithDatabase
) -}
where

import Control.Monad

import Control.Parallel.Strategies

import Data.List 
import Data.Maybe
import Data.Binary hiding (Word)

import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL -- hiding (head, map, foldl)

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Holumbus.Index.Common

import Holumbus.Control.MapReduce.MapReducible

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

-- import Holumbus.Utility

import Text.XML.HXT.Arrow.Pickle.Xml

import Database.HDBC
import Database.HDBC.Sqlite3

-- | The index consists of a table which maps documents to ids and a number of index parts.
data InvertedM = InvertedM 
  { indexParts  :: Parts      -- ^ The parts of the index, each representing one context.
  , path        :: FilePath   -- ^ Path to a directory containing occurences data
  , connection  :: Connection
  , nextId      :: Int
  } -- deriving (Show, Eq)

instance Show InvertedM where
  show i = "InvertedM{ path="++ show (path i)  ++ " ; indexParts=" ++ show (indexParts i) ++"}"

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part
-- | The index part is the real inverted index. Words are mapped to a file pointer.
type Part        = StrMap Int

type DatabaseIndex = Int

instance MapReducible InvertedM Context (Word, DocId, Position) where
  mergeMR         = undefined -- mergeIndexes
  reduceMR _ _ _  = undefined -- return $ Just $ foldl' (\i (w, d, p) -> insertPosition c w d p i) emptyInverted os 

instance HolIndexM IO InvertedM where
  sizeWordsM i = foldM (\c p -> return $ c + (SM.size p)) 0 (map snd . M.toList $ indexParts i)
  contextsM = return . map fst . M.toList . indexParts

  allWordsM     i c   = mapM (rawHelper i) $ SM.toList                      $ getPart c i
  prefixCaseM   i c q = mapM (rawHelper i) $ SM.prefixFindWithKey q         $ getPart c i
  prefixNoCaseM i c q = mapM (rawHelper i) $ SM.prefixFindNoCaseWithKey q   $ getPart c i
  lookupCaseM   i c q = mapM (rawHelper i) $ zip (repeat q) (maybeToList (SM.lookup q $ getPart c i))
  -- lookupCaseM   i c q = mapM (\di -> rawHelper i q di) $ maybeToList (SM.lookup q     $ getPart c i)
  lookupNoCaseM i c q = mapM (rawHelper i) $ SM.lookupNoCase q              $ getPart c i

  mergeIndexesM i1 i2 = do; l <- toListM i2; foldM (\i (c,w,o) -> insertOccurrencesM c w o i) i1 l

  insertOccurrencesM c w o i 
    =   let part = M.findWithDefault SM.empty c (indexParts i) 
            conn = connection i in
        if (not $ SM.member w part)
          then do
               storeOcc i (nextId i) o
               return i { indexParts = M.insertWith (SM.union) c (SM.singleton w (nextId i) ) (indexParts i)
                        , nextId = (nextId i) + 1 
                        }
          else do  -- the interesting case. occurences for the word already exist 
               di  <- SM.lookup w part
               occ <- retrieveOcc i di
               storeOcc i di (mergeOccurrences o occ)
               return i

  deleteOccurrencesM _ _ _ _ = error "Holumbus.Index.Inverted.Database: deleteOccurences not YET supported"

  updateDocIdsM _ _ = error "Holumbus.Index.Inverted.Database: updateDocIds not YET supported"
  
  toListM i = foldM convertPart [] (M.toList $ indexParts i) 
    where 
    convertPart :: [(Context, Word, Occurrences)] -> (Context, Part) -> IO [(Context, Word, Occurrences)]
    convertPart l (c,p) = do
                          res <- mapM (\(w, di) -> do; o <- retrieveOcc i di; return (c, w, o)) $ SM.toList $ p
                          return $ res ++ l

t = (IM.singleton 1 (IS.singleton 43))

{-
instance XmlPickler InvertedM where
  xpickle = xpZero

instance NFData InvertedM where
  rnf (InvertedM parts _ _ ) = rnf parts
-}
emptyInvertedM :: FilePath -> IO InvertedM
emptyInvertedM f = do; conn <- createConnection f; return $ InvertedM M.empty f conn 1

createConnection :: FilePath -> IO Connection
createConnection f =   do
                       conn <- connectSqlite3 f
                       t <- getTables conn
                       if "holumbus_occurrences" `elem` t
                         then error "Holumbus.Index.Inverted.Database: Table holumbus_occurrences already exists in this database"
                         else quickQuery conn createQuery [] >> commit conn >> return ()
                       return (conn)
  
-- | Return a part of the index for a given context.
getPart :: Context -> InvertedM -> Part
getPart c i = fromMaybe SM.empty (M.lookup c $ indexParts i)

rawHelper :: InvertedM -> (Word, DatabaseIndex) -> IO (Word, Occurrences)
rawHelper i (w, di) = do; o <- retrieveOcc i di; return (w, o)

-- | Read occurrences from database
retrieveOcc :: InvertedM -> Int -> IO Occurrences
retrieveOcc i di = let conn = connection i in
                   do
                   r <- quickQuery conn lookupQuery [toSql di] 
                   return $ decode $ BL.fromChunks [fromSql $ head $ head r]

storeOcc :: InvertedM -> Int -> Occurrences -> IO ()
storeOcc i di o = let conn = connection i in 
                  quickQuery conn insertQuery [toSql (di), toSql (B.concat . BL.toChunks $ encode o)] 
                  >> commit conn

createQuery :: String
createQuery = "CREATE TABLE holumbus_occurrences (id INTEGER PRIMARY KEY, occurrences BLOB)"

insertQuery :: String
insertQuery = "INSERT OR REPLACE INTO holumbus_occurrences (id, occurrences) VALUES (?, ?)"

lookupQuery :: String
lookupQuery = "SELECT occurrences FROM holumbus_occurrences WHERE (id = ?)"
