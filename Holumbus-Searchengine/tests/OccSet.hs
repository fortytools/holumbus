{-# LANGUAGE BangPatterns #-}
module OccSet where

import qualified Data.IntMap        as M
import qualified Data.IntMap.Strict as SM
import qualified Data.IntSet as S
import Data.Monoid (Monoid(..), (<>))

type DocId = Int
type Position = Int

type DocIdSet = S.IntSet
type Positions = S.IntSet

type DocIdMap = M.IntMap Positions
type DocIdMap1 = M.IntMap Position


data OccSet' = OS' DocIdSet DocIdMap1 DocIdMap
               deriving (Show)

instance Monoid OccSet' where
    mempty = OS' S.empty M.empty M.empty
    mappend (OS' o1 s1 m1) (OS' o2 s2 m2)
        = OS' (o1 `S.union` o2) s' m'
          where
            (s', m') = (s1, m1) `union0` (s2, m2)

toOccSet' :: DocIdMap -> OccSet'
toOccSet' m0
    = OS' o s m
      where
        (o, s , m) = SM.foldrWithKey split (S.empty, M.empty, M.empty) m0
        split k ps (o1, s1, m1)
            | p1 == 1	 = let res = (S.insert k o1, s1, m1)       -- pos == 1: throw occ into o1
                           in
                             if S.null ps1
                             then res
                             else split k ps1 res                  -- throw occ into s1 or m1
            | S.null ps1 = (o1, M.insert k p1 s1, m1)              -- throw occ into s1
            | otherwise  = (o1, s1, M.insert k ps m1)              -- throw occ into m1
            where
              (p1, ps1) = S.deleteFindMin ps

fromDocIdMap1 :: DocIdMap1 -> DocIdMap
fromDocIdMap1 = M.map S.singleton

toDocIdMap1 :: DocIdMap -> (DocIdMap1, DocIdMap)
toDocIdMap1 m
    = SM.foldrWithKey split (M.empty, m) m
    where
      split k ps (s1, m1)
          | S.null ps1 = (M.insert k p1 s1, M.delete k m1)
          | otherwise  = (              s1,            m1)
          where
            (p1, ps1) = S.deleteFindMin ps

union0 :: (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap)
union0 o1@(s1, m1) o2@(_s2, m2)
    | not (s1 `disjoint` m2) = union1 o1'  o2
    | otherwise              = union1 o1   o2
    where
      o1' = (s1', m1')
      s1' = s1 `difference` m2
      m1' = m1 `union` fromDocIdMap1 (s1 `difference` s1')


union1 :: (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap)
union1 o1@(_s1, m1) o2@(s2, m2)
    | not (s2 `disjoint` m1) = union2 o1 o2'
    | otherwise              = union2 o1 o2
    where
      o2' = (s2', m2')
      s2' = s2 `difference` m1
      m2' = m2 `union` fromDocIdMap1 (s2 `difference` s2')

union2 :: (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap)
union2 o1@(s1, m1) o2@(s2, m2)
    | not (s1 `disjoint` s2) = union3 o1 o2
    | otherwise              = (s1 `SM.union` s2, SM.unionWith S.union m1 m2)

union3 :: (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap) -> (DocIdMap1, DocIdMap)
union3 (s1, m1) (s2, m2)
    = toDocIdMap1 $ fromDocIdMap1 s1 `union` fromDocIdMap1 s2 `union` m1 `union` m2


union :: DocIdMap -> DocIdMap -> DocIdMap
union = M.unionWith S.union

union' :: DocIdMap -> DocIdMap -> DocIdMap
union' = SM.unionWith S.union

difference :: M.IntMap a -> M.IntMap b -> M.IntMap a
difference = M.difference

-- ----------------------------------------
 
disjoint :: M.IntMap a -> M.IntMap b -> Bool
disjoint x1 x2 = M.null (x1 `M.intersection` x2)

minus :: DocIdMap1 -> DocIdMap1 -> DocIdMap1
minus
    = M.differenceWith (\ p1 p2 -> if p1 == p2
                                   then Just p1
                                   else Nothing
                       )

-- ----------------------------------------

sm1, sm2 :: DocIdMap1
sm1 = M.fromList [(1,10),(2,20),(3,30),(4,1)]
sm2 = M.fromList [(2,20),(3,31),(4,40),(5,1),(1,1)]

mm1 :: DocIdMap
mm1 = M.unionWith S.union (fromDocIdMap1 sm1) (fromDocIdMap1 sm2)

occ1 :: OccSet'
occ1 = toOccSet' mm1


t1 :: (DocIdMap1, DocIdMap)
t1 = (sm1, M.empty) `union0` (sm2, M.empty)
