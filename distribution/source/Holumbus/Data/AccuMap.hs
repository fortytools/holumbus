-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Data.AccuMap
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A map of key-value-pairs. The values are hold in a list, so adding the same
  key-value-pair twice to the map, will result in storing the value twice.
  Even the sequence of adding the values will be saved.
  
  The name AccuMap is from accumulation map. You can use this map to easily
  storing all you key-value-pairs. After that you can all values-lists by
  key. Unlike the MultiMap, you don't lose the information of identical values
  and their order of adding.
  
  Most functions are inspired by the Data.Map type. 
-}
-- ----------------------------------------------------------------------------

module Holumbus.Data.AccuMap
(
  AccuMap
, empty
, null
, insert
, insertList
, lookup
, member
, deleteKey
, union
, fromList
, fromTupleList
, toList
)
where

import           Prelude hiding (null, lookup)

import qualified Data.Map as Map


-- | the AccuMap datatype
data AccuMap k a = AM (Map.Map k [a])
  deriving (Eq, Ord)


instance (Show k, Show a) => Show (AccuMap k a) where
  show (AM m) = msShow
    where
    ms = Map.toList m
    msShow = concat $ map (\(k,s) -> (show k) ++ "\n" ++ (showL s)) ms
    showL ls = concat $ map (\s -> show s ++ "\n") ls 


-- | Creates an empty AccuMap.
empty :: (Ord k) => AccuMap k a
empty = AM Map.empty


-- | Test, if AccuMap is empty.
null :: (Ord k) => AccuMap k a -> Bool
null (AM m) = Map.null m


-- | Insert a key-value-pair to the AccuMap.
insert :: (Ord k) => k -> a -> AccuMap k a -> AccuMap k a
insert k a (AM m) = AM $ Map.alter altering k m
  where
    altering Nothing = Just $ [a]
    altering (Just s) = Just $ a:s


-- | Insert a key and a list of values to the AccuMap.
--   Faster than adding all pair one by one.
insertList :: (Ord k) => k -> [a] -> AccuMap k a -> AccuMap k a
insertList _ [] m      = m
insertList k as (AM m) = AM $ Map.alter altering k m
  where
    altering Nothing = Just $ as
    altering (Just s) = Just $ as ++ s


-- | Get the list of values for one key. If the key doesn't exists,
--   an empty list is returned.
lookup :: (Ord k) => k -> AccuMap k a -> [a]
lookup k (AM m) = maybe [] (id) (Map.lookup k m)


-- | Test, if the key is in the AccuMap.
member :: (Ord k, Eq a) => k -> AccuMap k a -> Bool
member k m = [] /= lookup k m


-- | Deletes a key and all its values from the AccuMap.
deleteKey :: (Ord k) => k -> AccuMap k a -> AccuMap k a
deleteKey k (AM m) = AM $ Map.delete k m


-- | Combines two AccuMaps, the ordering of the two maps is
--   significant for the order of the values-elements.
union :: (Ord k) => AccuMap k a -> AccuMap k a -> AccuMap k a
union (AM m1) (AM m2) = AM $ Map.unionWith (\l1 l2 -> l1 ++ l2) m1 m2 


-- | Creates an AccuMap from a list.
fromList :: (Ord k) => [(k,[a])] -> AccuMap k a
fromList ks = foldl (\m (k,as) -> insertList k as m) empty ks


-- | Creates an AccuMap from a tuple list.
fromTupleList :: (Ord k) => [(k,a)] -> AccuMap k a
fromTupleList ts = fromList $ map (\(k,a) -> (k,[a])) ts


-- | Transforms an AccuMap to a list.
toList :: (Ord k) => AccuMap k a -> [(k,[a])]
toList (AM m) = Map.toList m