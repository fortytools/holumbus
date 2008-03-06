-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Cache
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A persistent full-text cache for arbitrary documents.
  
  Implemented using the single file database SQLite.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Cache 
  (
  -- * Cache types
  Cache
  
  -- * Construction
  , createCache
  )
where

import Prelude hiding (lookup)

import System.IO

import Database.HDBC
import Database.HDBC.Sqlite3

import Holumbus.Index.Common

-- | A simple document cache based on a SQLite database.
data Cache = Cache Connection

instance HolCache Cache where
  getDocText (Cache conn) c d = handleSql (\_ -> return Nothing) lookup
    where
    lookup = do
             r <- quickQuery conn lookupQuery [toSql d, toSql c]
             if null r then return Nothing else return (fromSql $ head $ head r)

  putDocText (Cache conn) c d t = handleSql (\_ -> return ()) insert
    where
    insert = do
             quickQuery conn insertQuery [toSql d, toSql c, toSql t]
             commit conn

-- | The query for creating the database table.    
createQuery :: String
createQuery = "CREATE TABLE holumbus_cache (id INTEGER, context TEXT, content TEXT)"

-- | The query for inserting an entry into the table.
insertQuery :: String
insertQuery = "INSERT OR REPLACE INTO holumbus_cache (id, context, content) VALUES (?, ?, ?)"

-- | The query for retrieving an entry from the table.
lookupQuery :: String
lookupQuery = "SELECT content FROM holumbus_cache WHERE (id = ?) AND (context = ?)"

-- | Creates a new document cache from a given database file. If the database exists already, 
-- nothing is changed. If the database does not yet exist, it is properly initialized. The cache
-- should only be created once for every database file during the lifetime of a program.
createCache :: FilePath -> IO Cache
createCache f = do
                conn <- connectSqlite3 f
                t <- getTables conn
                if "holumbus_cache" `elem` t then return () 
                  else quickQuery conn createQuery [] >> commit conn >> return ()
                return (Cache conn)
