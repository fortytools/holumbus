{-# OPTIONS -XTypeSynonymInstances -XMultiParamTypeClasses #-}
-- ----------------------------------------------------------------------------


{- |
  Module     : Holumbus.Index.Inverted.Database
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
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
    (
     -- * Persistent index types
      InvertedM
    , emptyInvertedM
    , makeInvertedM
    )
where

import           Control.Monad

import           Data.Binary            hiding (Word)
import qualified Data.ByteString        as B 
import qualified Data.ByteString.Lazy   as BL -- hiding (head, map, foldl)
-- import           Data.List 
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe

import           Database.HDBC
import           Database.HDBC.Sqlite3

import           Holumbus.Data.StrMap   (StrMap)
import qualified Holumbus.Data.StrMap   as SM
import           Holumbus.Index.Common
import           Holumbus.Index.Inverted.Memory (Inverted)

import           System.IO.Unsafe

-- ------------------------------------------------------------

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts       = Map Context Part

-- | The index part is the real inverted index. Words are mapped to a file pointer.
type Part        = StrMap Int

type DatabaseIndex = Int

-- ------------------------------------------------------------

-- | The index consists of a table which maps documents to ids and a number of index parts.
data InvertedM = InvertedM 
  { indexParts  :: Parts      -- ^ The parts of the index, each representing one context.
  , path        :: FilePath   -- ^ Path to a db-file
  , nextId      :: Int
  , connection  :: Connection
  } -- deriving (Show, Eq)

-- ------------------------------------------------------------

instance Binary InvertedM where
  put (InvertedM ps pa ni _ ) = put ps >> put pa >> put ni
  get = do
        ps  <- get
        pa  <- get
        ni <- get
        return $! InvertedM ps pa ni (unsafePerformIO (createConnection pa)) 
--         undefined -- liftM2 InvertedM get get 

-- ------------------------------------------------------------

instance Show InvertedM where
  show i = "InvertedM{ path="++ show (path i)  ++ " ; indexParts=" ++ show (indexParts i) ++"}"

-- ------------------------------------------------------------

instance HolIndexM IO InvertedM where
  sizeWordsM i          = foldM (\c p -> return $ c + (SM.size p)) 0 (map snd . M.toList $ indexParts i)
  contextsM             = return . map fst . M.toList . indexParts

  allWordsM     i c     = mapM (rawHelperM i) $ SM.toList                      $ getPart c i
  prefixCaseM   i c q   = mapM (rawHelperM i) $ SM.prefixFindWithKey q         $ getPart c i
  prefixNoCaseM i c q   = mapM (rawHelperM i) $ SM.prefixFindNoCaseWithKey q   $ getPart c i
  lookupCaseM   i c q   = mapM (rawHelperM i) $ zip (repeat q) (maybeToList (SM.lookup q $ getPart c i))
  lookupNoCaseM i c q   = mapM (rawHelperM i) $ SM.lookupNoCase q              $ getPart c i

  mergeIndexesM i1 i2   = do
                          l <- toListM' i2
                          foldM (\i (c,w,o) -> insertOccurrencesM' c w o i) i1 l

  insertOccurrencesM    =  insertOccurrencesM'

  deleteOccurrencesM _ _ _ _ 	= error "Holumbus.Index.Inverted.Database: deleteOccurences not YET supported"
  updateDocIdsM _ _     	= error "Holumbus.Index.Inverted.Database: updateDocIds not YET supported"
  updateDocIdsM' _ _     	= error "Holumbus.Index.Inverted.Database: updateDocIds' not YET supported"
  
  toListM               = toListM'

-- the ' functions are neccessary to prevent possible "overlapping instances" errors

insertOccurrencesM'     :: Context -> String -> Occurrences -> InvertedM -> IO InvertedM
insertOccurrencesM' c w o i 
    =   let part = M.findWithDefault SM.empty c (indexParts i) in 
        if (not $ SM.member w part)
          then storeOccM i (nextId i) o
               >>
               return i { indexParts = M.insertWith (SM.union) c (SM.singleton w (nextId i) ) (indexParts i)
                        , nextId = (nextId i) + 1 
                        }
          else do  -- the interesting case. occurences for the word already exist 
               di  <- SM.lookup w part
               occ <- retrieveOcc i di
               storeOccM i di (mergeOccurrences o occ)
                >> return i

toListM'                :: InvertedM -> IO [(Context, Word, Occurrences)]
toListM' i              = foldM convertPart [] (M.toList $ indexParts i) 
    where 
    convertPart         :: [(Context, Word, Occurrences)] -> (Context, Part) -> IO [(Context, Word, Occurrences)]
    convertPart l (c,p) = do
                          res <- mapM ( \ (w, di) ->
                                        do
                                        o <- retrieveOcc i di
                                        return (c, w, o)
                                      ) $ SM.toList $ p
                          return $ res ++ l

-- ------------------------------------------------------------

makeInvertedM           :: FilePath -> Inverted -> IO InvertedM
makeInvertedM f i       = do
                          i' <- emptyInvertedM f
                          foldM (\i'' (c,w,o) -> insertOccurrencesM' c w o i'') i' (toList i) 
    
emptyInvertedM          :: FilePath -> IO InvertedM
emptyInvertedM f        = do
                          conn <- createConnection f
                          return $ InvertedM M.empty f 1 conn

createConnection        :: FilePath -> IO Connection
createConnection f      = do
                          conn <- connectSqlite3 f
                          t <- getTables conn
                          if "holumbus_occurrences" `elem` t
                             then return () -- error "Holumbus.Index.Inverted.Database: Table holumbus_occurrences already exists in this database"
                             else quickQuery conn createQuery [] >> commit conn >> return ()
                          return (conn)
  
-- | Return a part of the index for a given context.
getPart                 :: Context -> InvertedM -> Part
getPart c i             = fromMaybe SM.empty (M.lookup c $ indexParts i)

rawHelperM              :: InvertedM -> (Word, DatabaseIndex) -> IO (Word, Occurrences)
rawHelperM i (w, di)    = do
                          o <- retrieveOcc i di
                          return (w, o)

-- | Read occurrences from database
retrieveOcc             :: InvertedM -> Int -> IO Occurrences
retrieveOcc i di        = let
                          conn = connection i
                          in
                          do
                          r <- quickQuery conn lookupQuery [toSql di] 
                          return $ decode $ BL.fromChunks [fromSql $ head $ head r]

storeOccM               :: InvertedM -> Int -> Occurrences -> IO (Int)
storeOccM i di o        = let
                          conn = connection i
                          in
                          storeOccM' conn di o

  
storeOccM'              :: Connection -> Int -> Occurrences -> IO (Int)
storeOccM' conn di o    =  quickQuery conn 
                                      insertQuery
                                      [toSql (di), toSql (B.concat . BL.toChunks $ encode o)] 
                           >> commit conn
                           >> return di
  
createQuery             :: String
createQuery             = "CREATE TABLE holumbus_occurrences (id INTEGER PRIMARY KEY, occurrences BLOB)"

insertQuery             :: String
insertQuery             = "INSERT OR REPLACE INTO holumbus_occurrences (id, occurrences) VALUES (?, ?)"

lookupQuery             :: String
lookupQuery             = "SELECT occurrences FROM holumbus_occurrences WHERE (id = ?)"

-- ------------------------------------------------------------
