{-# LANGUAGE BangPatterns #-}

module OccSet where

import           Control.DeepSeq

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet        as S
import           Data.Monoid        (Monoid (..), (<>))
import qualified Data.Vector        as V

type DocId       = Int
type Position    = Int

type Positions   = S.IntSet		-- set of positions
type OccMap      = M.IntMap Positions  -- set of pairs (doc id, set of position)
type SingleOccs  = M.IntMap Position   -- set of pairs (doc id, position)
type FstOccs     = S.IntSet	   	-- set of doc ids

data OccSet      = Empty
                 | Firsts    ! FstOccs
                 | Singles   ! SingleOccs  ! OccSet
                 | Multiples ! OccMap ! OccSet
                 deriving (Show)

-- ------------------------------------------------------------

intCacheSize :: Int
intCacheSize = 1024

intCache :: V.Vector Int
intCache = let v = V.generate intCacheSize id in rnf v `seq` v

share :: Int -> Int			-- java like: share all Ints `elem` [0..255]
share !i
    | 0 <= i && i < intCacheSize = let j = intCache V.! i in j `seq` j
    | otherwise                  =                                   i

one :: Int
one = share 1

-- ------------------------------------------------------------
--
-- naming convention
-- f'*** : operations on sets of doc ids
-- p'*** : operations on pairs (SingleOccs, OccMap)
-- o'*** : operations on Occurences
-- s'*** : operations on SingleOccs
-- o3'** : operations on triples Occs3
-- os'** : operations on OccSet

-- ------------------------------------------------------------

os'empty :: OccSet
os'empty = Empty

os'singleton :: DocId -> Position -> OccSet
os'singleton d p
    | p == one  = Firsts  $ f'singleton d
    | otherwise = Singles  (s'singleton d (share p)) Empty

os'size :: OccSet -> Int
os'size = o3'size . os'toOccs3

os'insert :: DocId -> Position -> OccSet -> OccSet
os'insert d p = os'iso'map'o3 $ o3'insert d (share p)

os'delete :: DocId -> Position -> OccSet -> OccSet
os'delete d p = os'iso'map'o3 $ o3'delete d p

os'update :: (DocId -> DocId) -> OccSet -> OccSet
os'update f = os'iso'map'o $ o'update f

os'union :: OccSet -> OccSet -> OccSet
os'union = os'iso'zip'o3 o3'union

os'differenceAll :: OccSet -> OccSet -> OccSet
os'differenceAll = os'iso'zip'o o'differenceAll

os'intersection :: OccSet -> OccSet -> OccSet
os'intersection = os'iso'zip'o o'intersection

-- ----------------------------------------

os'fromOccMap :: OccMap -> OccSet
os'fromOccMap = os'fromOccs3 . o3'fromOccMap

os'toOccMap :: OccSet -> OccMap
os'toOccMap = o3'toOccMap . os'toOccs3

os'iso'map'o :: (OccMap -> OccMap) -> OccSet -> OccSet
os'iso'map'o = iso'map os'toOccMap os'fromOccMap

os'iso'zip'o :: (OccMap -> OccMap -> OccMap) -> OccSet -> OccSet -> OccSet
os'iso'zip'o = iso'zip os'toOccMap os'fromOccMap

-- ----------------------------------------

os'fromOccs3 :: Occs3 -> OccSet
os'fromOccs3 (f, (s, m))
    = r3
    where
      r1 | f'null f  = Empty
         | otherwise = Firsts f
      r2 | s'null s  = r1
         | otherwise = Singles s r1
      r3 | o'null m  = r2
         | otherwise = Multiples m r2

os'toOccs3 :: OccSet -> Occs3
os'toOccs3 Empty 	    = o3'empty
os'toOccs3 (Firsts f)       = (f, (s'empty, o'empty))
os'toOccs3 (Singles s os)   = (f, (s, m))
    where (f, (_, m))       = os'toOccs3 os
os'toOccs3 (Multiples m os) = (f, (s, m))
    where (f, (s, _))       = os'toOccs3 os

os'iso'map'o3 :: (Occs3 -> Occs3) -> OccSet -> OccSet
os'iso'map'o3 = iso'map os'toOccs3 os'fromOccs3

os'iso'zip'o3 :: (Occs3 -> Occs3 -> Occs3) -> OccSet -> OccSet -> OccSet
os'iso'zip'o3 = iso'zip os'toOccs3 os'fromOccs3

-- ------------------------------------------------------------
--
-- auxiliary type for access of the 3 tables for the positions

type Occs3 = (FstOccs, OccsPair)

o3'inv :: Occs3 -> Bool
o3'inv (_f, (s, m))
    = s `disjoint` m
      &&
      undefined -- all position sizes are > 1
                -- no positions <= 1 in s an m

o3'empty :: Occs3
o3'empty = (f'empty, (s'empty, o'empty))

o3'size :: Occs3 -> Int
o3'size (f, (s, m))
    = sf + ss + sm
      where
        sf = S.size f
        ss = M.size s
        sm = M.foldr' ((+) . S.size) 0 m

o3'insert :: DocId -> Position -> Occs3 -> Occs3
o3'insert d p (f, pp)
    | p == one	= (f'insert d f,              pp)
    | otherwise = (           f, p'insert d p pp)

o3'delete :: DocId -> Position -> Occs3 -> Occs3
o3'delete d p (f, pp)
    | p == one	= (f'delete d f,              pp)
    | otherwise = (           f, p'delete d p pp)


o3'union :: Occs3 -> Occs3 -> Occs3
o3'union (f1, p1) (f2, p2)
    = (f1 `f'union` f2, p1 `p'union0` p2)

o3'fromOccMap :: OccMap -> Occs3
o3'fromOccMap m0
    = M.foldrWithKey split o3'empty m0
      where
        split k ps (o1, pp1@(s1, m1))
            | p1 == one	 = let res = (S.insert k o1, pp1)          -- pos == 1: throw occ into o1
                           in
                             if S.null ps1
                             then res
                             else split k ps1 res                  -- throw occ into s1 or m1
            | S.null ps1 = (o1, (M.insert k p1 s1, m1))           -- throw occ into s1
            | otherwise  = (o1, (s1, M.insert k ps m1))           -- throw occ into m1
            where
              (p1, ps1) = S.deleteFindMin ps

o3'toOccMap :: Occs3 -> OccMap
o3'toOccMap (f, p)
    = f'toOccMap f `o'union` p'toOccMap p

-- ------------------------------------------------------------
--
-- operations on docid sets for the first occurrences

f'empty 	:: FstOccs
f'empty 	= S.empty

f'singleton     :: DocId -> FstOccs
f'singleton     = S.singleton

f'insert	:: DocId -> FstOccs -> FstOccs
f'insert	= S.insert

f'delete	:: DocId -> FstOccs -> FstOccs
f'delete	= S.delete

f'union 	:: FstOccs -> FstOccs -> FstOccs
f'union 	= S.union

f'null  	:: FstOccs -> Bool
f'null  	= S.null

f'toOccMap 	:: FstOccs -> OccMap
f'toOccMap 	= S.foldr (\ d res -> o'insert d one res) o'empty

-- ------------------------------------------------------------
--
-- operations on (docid, position) pairs

s'empty 	:: SingleOccs
s'empty 	= M.empty

s'singleton     :: DocId -> Position -> SingleOccs
s'singleton     = M.singleton

s'null 		:: SingleOccs -> Bool
s'null 		= M.null

s'delete	:: DocId -> SingleOccs -> SingleOccs
s'delete	= M.delete

s'toOccMap 	:: SingleOccs -> OccMap
s'toOccMap 	= M.map S.singleton

s'minus 	:: SingleOccs -> SingleOccs -> SingleOccs
s'minus 	= M.differenceWith
                  (\ p1 p2 -> if p1 == p2
                              then Just p1
                              else Nothing
                  )

-- ------------------------------------------------------------
--
-- operations on occurrence maps

o'empty 	:: OccMap
o'empty 	= M.empty

o'null 		:: OccMap -> Bool
o'null 		= M.null

o'insert	:: DocId -> Position -> OccMap -> OccMap
o'insert d p    = M.insertWith S.union d (S.singleton p)

o'delete	:: DocId -> Position -> OccMap -> OccMap
o'delete d p    = M.update del d
    where
      del s	= let s' = S.delete p s in
                  if S.null s' then Nothing else Just s'

o'deleteAll	:: DocId -> OccMap -> OccMap
o'deleteAll     = M.delete

o'union 	:: OccMap -> OccMap -> OccMap
o'union 	= M.unionWith S.union

o'union' 	:: OccMap -> OccMap -> OccMap
o'union' 	= M.unionWith S.union

o'update        :: (DocId -> DocId) -> OccMap -> OccMap
o'update f	= M.foldrWithKey
                  (\ d ps res -> M.insertWith S.union (f d) ps res) o'empty

o'differenceAll	:: OccMap -> OccMap -> OccMap
o'differenceAll	= M.difference

o'difference	:: OccMap -> OccMap -> OccMap
o'difference	= M.differenceWith diff
    where
      diff ps1 ps2
          | S.null res = Nothing
          | otherwise  = Just res
          where
            res = S.difference ps1 ps2

o'intersection	:: OccMap -> OccMap -> OccMap
o'intersection	= M.intersectionWith S.union

o'intersections	:: [OccMap] -> OccMap
o'intersections	= foldr o'intersection o'empty

o'phrase2 	:: OccMap -> OccMap -> OccMap
o'phrase2	= M.mergeWithKey (const combine) (const M.empty) (const M.empty)
    where
      combine ps1 ps2
		= if S.null ps
                  then Nothing
                  else Just ps
          where
            ps	= ps1 `S.intersection` (S.map (\ x -> x - 1) ps2)

o'phrase 	:: [OccMap] -> OccMap
o'phrase []	= o'empty
o'phrase ms	= foldr1 o'phrase2 ms		-- the "r" in "foldr1" is important

-- ------------------------------------------------------------
--
-- operations on pairs of single occurrences and occurrences

type OccsPair = (SingleOccs, OccMap)

p'fromOccMap :: OccMap -> OccsPair
p'fromOccMap m
    = M.foldrWithKey split (M.empty, m) m
    where
      split k ps (s1, m1)
          | S.null ps1 = (M.insert k p1 s1, M.delete k m1)
          | otherwise  = (               s1,             m1)
          where
            (p1, ps1) = S.deleteFindMin ps

p'toOccMap :: OccsPair -> OccMap
p'toOccMap (s, m)
    = s'toOccMap s `o'union` m

p'insert :: DocId -> Position -> OccsPair -> OccsPair
p'insert d p (s, m)
    | M.member d m = (s, M.adjust (S.insert p) d m)
    | otherwise = case M.lookup d s of
                    Nothing -> (M.insert d p s, m)
                    Just p1
                        | p1 /= p
                            -> (M.delete d s, M.adjust (S.insert p) d m)
                        | otherwise
                            -> (s, m)

p'delete :: DocId -> Position -> OccsPair -> OccsPair
p'delete d p
    = p'fromOccMap . o'delete d p . p'toOccMap	-- the easy but inefficient way

p'deleteAll :: DocId -> OccsPair -> OccsPair
p'deleteAll d (s, m)
    = (s'delete d s, o'deleteAll d m)

p'union0 :: OccsPair -> OccsPair -> OccsPair
p'union0 o1@(s1, m1) o2@(_s2, m2)
    | not (s1 `disjoint` m2) = p'union1 o1'  o2
    | otherwise              = p'union1 o1   o2
    where
      o1' = (s1', m1')
      s1' = s1 `difference` m2
      m1' = m1 `o'union` s'toOccMap (s1 `difference` s1')


p'union1 :: OccsPair -> OccsPair -> OccsPair
p'union1 o1@(_s1, m1) o2@(s2, m2)
    | not (s2 `disjoint` m1) = p'union2 o1 o2'
    | otherwise              = p'union2 o1 o2
    where
      o2' = (s2', m2')
      s2' = s2 `difference` m1
      m2' = m2 `o'union` s'toOccMap (s2 `difference` s2')

p'union2 :: OccsPair -> OccsPair -> OccsPair
p'union2 o1@(s1, m1) o2@(s2, m2)
    | not (s1 `disjoint` s2) = p'union3 o1 o2
    | otherwise              = (s1 `M.union` s2, M.unionWith S.union m1 m2)

p'union3 :: OccsPair -> OccsPair -> OccsPair
p'union3 (s1, m1) (s2, m2)
    = p'fromOccMap $ s'toOccMap s1 `o'union` s'toOccMap s2 `o'union` m1 `o'union` m2

-- ----------------------------------------

difference :: M.IntMap a -> M.IntMap b -> M.IntMap a
difference = M.difference

disjoint :: M.IntMap a -> M.IntMap b -> Bool
disjoint x1 x2 = M.null (x1 `M.intersection` x2)

iso'map :: (a -> b) -> (b -> c) -> (b -> b) -> (a -> c)
iso'map to from op = from . op . to

iso'zip :: (a -> b) -> (b -> c) -> (b -> b -> b) -> (a -> a -> c)
iso'zip to from op2 = \ x y -> from $ to x `op2` to y

-- ----------------------------------------

sm1, sm2, sm3 :: SingleOccs
sm1 = M.fromList [(1,10),(2,20),(3,30),(4,1)]
sm2 = M.fromList [(2,20),(3,31),(4,40),(5,1),(1,1)]
sm3 = M.fromList [(1,11),(2,21),(3,31)]

sm :: [SingleOccs]
sm = [sm1,sm2,sm3]

mm :: [OccMap]
mm@[mm1,mm2,mm3] = map s'toOccMap [sm1,sm2,sm3]

mm4 :: OccMap
mm4 = mm1 `o'union` mm2

os1 :: OccSet
os1 = os'fromOccMap mm1

occ1 :: Occs3
occ1 = o3'fromOccMap mm1


t1 :: OccsPair
t1 = (sm1, M.empty) `p'union0` (sm2, M.empty)

-- ----------------------------------------
