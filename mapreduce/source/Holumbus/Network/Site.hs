-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Network.Site
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Just a little Id to help us to decide if two thread are running in the
  same program or just on the same machine or on differen machines. 

-}

-- ----------------------------------------------------------------------------


module Holumbus.Network.Site
(
-- * Datatypes
  SiteId
, SiteMap
  
-- * Operations on the SiteId
, getSiteId
, getSiteHost
, getSiteProcess
, isSameHost
, isSameProcess
, nearestId

-- * Operations on the SiteMap
, emptySiteMap
, addIdToMap
, deleteIdFromMap
, deleteHostFromMap
, isSiteIdMember
, getNeighbourSiteIds
)
where

import Data.Binary
import qualified Data.List as List
import Network.Socket
import System.Posix

import qualified Data.Map as Map
import qualified Data.Set as Set

-- ----------------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------------


-- | the datatype of the SiteId, it contains the hostname and a processid,
--   so it is possible to decide if two site ids belong to the same process
--   or the the same computer or are on distinct computers
data SiteId = SiteId HostName ProcessID deriving (Show, Eq, Ord)


instance Binary SiteId where
  put (SiteId hn pid) = put hn >> put (toInteger pid)
  get
    = do
      hn <- get
      pid <- get
      return (SiteId hn (fromInteger pid)) 

-- | Just a little Map to hold the SiteIds an to get the neighbout Ids
type SiteMap = Map.Map HostName (Set.Set SiteId) 

-- ----------------------------------------------------------------------------
-- Operations on the SiteId
-- ----------------------------------------------------------------------------


-- | little helper function
getHostName :: IO (HostName)
getHostName
  = do
    (hn, _) <- getNameInfo [] True False (SockAddrUnix "localhost")
    return (maybe "localhost" id hn)
    

-- | gets the SiteId for the calling program 
getSiteId :: IO (SiteId)
getSiteId
  = do
    hn <- getHostName
    pid <- getProcessID
    return (SiteId hn pid)

       
-- | extracts the Hostname from the SiteId
getSiteHost :: SiteId -> HostName
getSiteHost (SiteId hn _) = hn


-- | extracts the ProcessID from the SiteId
getSiteProcess :: SiteId -> ProcessID
getSiteProcess (SiteId _ pid) = pid


-- | Test, if the two Ids are located on the same host.
isSameHost :: SiteId -> SiteId -> Bool
isSameHost (SiteId hn1 _) (SiteId hn2 _) = hn1 == hn2


-- | Test, if the two Ids are located on the same host an in the same process.
isSameProcess :: SiteId -> SiteId -> Bool
isSameProcess = (==)


filterSiteIds :: SiteId -> [SiteId] -> ([SiteId],[SiteId],[SiteId])
filterSiteIds _ [] = ([],[],[])
filterSiteIds i ls
  = (same, local, other)
  where
    (same, temp) = List.partition (\s -> isSameProcess i s) ls
    (local, other) = List.partition (\s -> isSameHost i s) temp


nearestId :: SiteId -> [SiteId] -> Maybe SiteId
nearestId s l = nearestId' $ filterSiteIds s l
  where
  nearestId' ([],  [],  [])  = Nothing
  nearestId' ([],  [],  x:_) = Just x
  nearestId' ([],  x:_, _)   = Just x
  nearestId' (x:_, _,   _)   = Just x
-- ----------------------------------------------------------------------------
-- Operations on the SiteMap
-- ----------------------------------------------------------------------------


-- | empty SiteId-Map
emptySiteMap :: SiteMap
emptySiteMap = Map.empty

-- | adds an id to the map
addIdToMap :: SiteId -> SiteMap -> SiteMap
addIdToMap i m 
  = Map.alter f hn m
    where
      hn = getSiteHost i
      f Nothing = (Just $ Set.singleton i)
      f (Just s) = (Just $ Set.insert i s)


-- | deletes an id from the map
deleteIdFromMap :: SiteId -> SiteMap -> SiteMap
deleteIdFromMap i m
  = Map.alter f hn m
    where
      hn = getSiteHost i
      f Nothing = Nothing
      f (Just s) = filterEmpty $ Set.delete i s
      filterEmpty s
        | s == Set.empty = Nothing
        | otherwise = Just s


-- | delete a hostname an all its ids from the map
deleteHostFromMap :: HostName -> SiteMap -> SiteMap
deleteHostFromMap hn m
  = Map.alter f hn m
    where
      f _ = Nothing


-- | test, if the site id is already in the list
isSiteIdMember :: SiteId -> SiteMap -> Bool 
isSiteIdMember i m
  = maybe False (\s -> Set.member i s) (Map.lookup hn m) 
    where
      hn = getSiteHost i


-- | gets all ids which are on the same host, but not
--   the original siteid itself
getNeighbourSiteIds :: SiteId -> SiteMap -> Set.Set SiteId
getNeighbourSiteIds i m
  = maybe (Set.empty) (\s -> Set.delete i s) (Map.lookup hn m) 
    where
      hn = getSiteHost i