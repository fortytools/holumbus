-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Data.AccuMap
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


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
, toList
, fromList
, fromTupleList
)
where

import           Prelude hiding (null, lookup)

import qualified Data.Map as Map



data AccuMap k a = AM (Map.Map k [a])
  deriving (Eq, Ord)


instance (Show k, Show a) => Show (AccuMap k a) where
  show (AM m) = msShow
    where
    ms = Map.toList m
    msShow = concat $ map (\(k,s) -> (show k) ++ "\n" ++ (showL s)) ms
    showL ls = concat $ map (\s -> show s ++ "\n") ls 



empty :: (Ord k) => AccuMap k a
empty = AM Map.empty


null :: (Ord k) => AccuMap k a -> Bool
null (AM m) = Map.null m

 
insert :: (Ord k) => k -> a -> AccuMap k a -> AccuMap k a
insert k a (AM m) = AM $ Map.alter altering k m
  where
    altering Nothing = Just $ [a]
    altering (Just s) = Just $ a:s


insertList :: (Ord k) => k -> [a] -> AccuMap k a -> AccuMap k a
insertList _ [] m      = m
insertList k as (AM m) = AM $ Map.alter altering k m
  where
    altering Nothing = Just $ as
    altering (Just s) = Just $ as ++ s


lookup :: (Ord k) => k -> AccuMap k a -> [a]
lookup k (AM m) = maybe [] (id) (Map.lookup k m)


member :: (Ord k, Eq a) => k -> AccuMap k a -> Bool
member k m = [] == lookup k m


deleteKey :: (Ord k) => k -> AccuMap k a -> AccuMap k a
deleteKey k (AM m) = AM $ Map.delete k m


union :: (Ord k) => AccuMap k a -> AccuMap k a -> AccuMap k a
union (AM m1) (AM m2) = AM $ Map.unionWith (\l1 l2 -> l1 ++ l2) m1 m2 


toList :: (Ord k) => AccuMap k a -> [(k,[a])]
toList (AM m) = Map.toList m


fromList :: (Ord k) => [(k,[a])] -> AccuMap k a
fromList ks = foldl (\m (k,as) -> insertList k as m) empty ks


fromTupleList :: (Ord k) => [(k,a)] -> AccuMap k a
fromTupleList ts = fromList $ map (\(k,a) -> (k,[a])) ts