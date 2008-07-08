-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.MultiMap
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.MapReduce.MultiMap
(
  MultiMap
, empty
, null
, insert
, insertList
, insertSet
, lookup
-- , lookupKeys
-- , elems
, filterElements
, member
, delete
, deleteKey
, deleteElem
)
where

import Prelude hiding (null, lookup)

import qualified Data.Map as Map
import qualified Data.Set as Set


-- | a MultiMap, it can hold more (different!!!) Elements for one key
data MultiMap k a = MM (Map.Map k (Set.Set a))
  deriving (Show, Eq, Ord)


-- | the empty MultiMap
empty :: (Ord k, Ord a) => MultiMap k a
empty = MM Map.empty


-- | test, if the MultiMap is empty
null :: (Ord k, Ord a) => MultiMap k a -> Bool
null (MM m) = Map.null m


-- | inserts an element in the MultiMap
insert :: (Ord k, Ord a) => k -> a -> MultiMap k a -> MultiMap k a
insert k a (MM m) = MM $ Map.alter altering k m
  where
    altering Nothing = Just $ Set.singleton a
    altering (Just s) = Just $ Set.insert a s


-- | inserts multiple elements in a list to the MultiMap
insertList :: (Ord k, Ord a) => k -> [a] -> MultiMap k a -> MultiMap k a
insertList k as m = insertSet k (Set.fromList as) m
  

-- | inserts multiple elements in a set to the MultiMap
insertSet :: (Ord k, Ord a) => k -> Set.Set a -> MultiMap k a -> MultiMap k a
insertSet k newSet mm@(MM m) = 
  if (Set.null newSet) then mm else MM $ Map.alter altering k m
  where
    altering Nothing = Just newSet
    altering (Just s) = Just $ Set.union newSet s


-- | gets all different elements for one key or an empty set
lookup :: (Ord k, Ord a) => k -> MultiMap k a -> Set.Set a
lookup k (MM m) = maybe (Set.empty) (id) (Map.lookup k m)


-- | get all different elements from a list of keys
lookupKeys :: (Ord k, Ord a) => [k] -> MultiMap k a -> Set.Set a
lookupKeys ks m = Set.unions $ map (\k -> lookup k m) ks

elems :: (Ord k, Ord a) => MultiMap k a -> Set.Set a
elems (MM m) = Set.unions $ Map.elems m

filterElements :: (Ord k, Ord a) => [k] -> MultiMap k a -> Set.Set a
filterElements [] m = elems m  -- get all
filterElements ks m = lookupKeys ks m

-- | test, if a key is in the Map
member :: (Ord k, Ord a) => k -> MultiMap k a -> Bool
member k m = Set.empty == lookup k m

-- | deletes an Element from the Map, if the data in Nothing, the whole key is
--   deleted
delete :: (Ord k, Ord a) => k -> Maybe a -> MultiMap k a -> MultiMap k a
delete k Nothing m = deleteKey k m
delete k (Just a) m = deleteElem k a m


-- | deletes a whole key from the map
deleteKey :: (Ord k, Ord a) => k -> MultiMap k a -> MultiMap k a
deleteKey k (MM m) = MM $ Map.delete k m


-- | deletes a single Elemete from the map
deleteElem :: (Ord k, Ord a) => k -> a -> MultiMap k a -> MultiMap k a
deleteElem k a (MM m) = MM $ Map.alter delSet k m
  where
    delSet Nothing = Nothing
    delSet (Just set) = filterEmpty $ Set.delete a set
    filterEmpty set
      | set == Set.empty = Nothing
      | otherwise = Just set
