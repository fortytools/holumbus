{-# LANGUAGE BangPatterns #-}

-- ------------------------------------------------------------

module Main
where

import           Control.Arrow
import           Control.DeepSeq

import           Data.Map        (Map)
import qualified Data.Map        as M
import qualified Data.Map.Strict as SM
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S
-- {-
import           Debug.Trace
-- -}
-- ------------------------------------------------------------

type DAGList a                  = [(a, [a])]
type DAG a                      = Map a (Set a)
type Ranking a                  = Map a Score
type Score                      = Float

-- ------------------------------------------------------------

{-
-- | construct a directed graph form a list of nodes and successors.
--
--  This version does not check cycles, so it only works savely,
--  when this is checked before.

unsaveDagFromList               :: (Ord a, Show a) => DAGList a -> DAG a
unsaveDagFromList l             = -- traceShow l $
                                  map (second S.fromList) >>> M.fromList $ l
-- -}

-- | Construct a directed graph (DAG) form a list of nodes and successors.
--
-- The function checks for edges, that would introduce cycles, and deletes these
-- edges. So if there are cycles in the input list, the result depends on the
-- sequence of the pairs in the input list

-- with ghc-7.8.2 this function is evaluated repeatedly
-- a lot of times, visible by the traceShow in insertEdge,
-- with the insertion of the traceShow it's evalated only once,
-- as it should be and as it's done with ghc-7.6
--
-- substituting traceShow by a deepseq does not help

dagFromList                     :: (Ord a, Show a, NFData a) => DAGList a -> DAG a
dagFromList l                   = -- traceShow l $
                                  map (second S.fromList)
                                  >>>
                                  foldl (flip insEdges) M.empty $ l

insEdges                        :: (Ord a, Show a) => (a, Set a) -> DAG a -> DAG a
insEdges (x, ys) g
    | S.null ys                 = M.insertWith S.union x ys g
    | otherwise                 = S.fold (insertEdge x) g $ ys


-- these comments don't have influence on the bug,
-- it was a nice try

{-# NOINLINE dagFromList #-}
{-# NOINLINE insEdges #-}
{-# NOINLINE insertEdge #-}

-- ------------------------------------------------------------

-- | insert an edge from x to y into DAG g.
--
-- Check for possible cycles. Edges leading to cycles are discarded

insertEdge                      :: (Ord a, Show a) => a -> a -> DAG a -> DAG a
insertEdge x y g
    | x == y                    = traceShow ("cycle:", [x,y]) $
                                  g
    | existPath                 = traceShow ("cycle:", x:(head path)) $
                                  g
    | otherwise                 = M.insertWith S.union x (S.singleton y) g
    where
    path                        = take 1 $ allPaths g y x
    existPath                   = not . null $ path

-- ------------------------------------------------------------

-- | Compute all paths from one node to another.
--
-- this is used by insertEdge, when checking for cycles

allPaths                        :: (Ord a) => DAG a -> a -> a -> [[a]]
allPaths g                      = allPaths'
    where
    allPaths' x y
        | y `S.member` succs    = [[x,y]]
        | otherwise             = map (x:) . concatMap (flip allPaths' y) . S.toList $ succs
        where
        succs                   = fromMaybe S.empty . M.lookup x $ g

-- ------------------------------------------------------------

-- | Inverse to dagFromList

dagToList                       :: DAG a -> [(a, [a])]
dagToList                       = M.toList >>> map (second S.toList)

-- ------------------------------------------------------------

-- | Switch the direction in the DAG

dagInvert                       :: (Ord a) => DAG a -> DAG a
dagInvert                       = M.foldrWithKey invVs M.empty
    where
    invVs k ks acc              = S.fold invV acc1 $ ks
        where
        acc1                    = M.insertWith S.union k  S.empty         $ acc         -- don't forget the roots
        invV k' acc'            = M.insertWith S.union k' (S.singleton k) $ acc'

-- ------------------------------------------------------------

-- deepseq with g does not help, stil multiple evaluations of dagFromList and insertEdge

ranking                         :: (Ord a, Show a, NFData a) => Score -> DAG a -> Ranking a
ranking w g                     = -- traceShow r
                                  r
    where
    g'                          = {- g `deepseq` -} dagInvert g
    r                           = foldl insertRank M.empty $ M.keys g
        where
        insertRank r' k         = M.insert k (w * (S.fold accRank (1/w) usedBy)) r'
            where
            usedBy              = fromMaybe S.empty . M.lookup k $ g'
            accRank k' acc'     = ( fromJust . M.lookup k' $ r ) + acc'

rankingStd                      :: (Ord a, Show a, NFData a) => DAGList a -> Ranking a
rankingStd                      = SM.map scale . ranking deflate . dagFromList
    where
      scale                     = (/10.0) . fromInteger . round . (*10) . (+1.0) . logBase 2
      deflate                   = 0.5

-- ------------------------------------------------------------
{- minimal test case

d1 :: DAG Int
d1 = dagFromList [(1,[2,3])
                 ,(2,[3,4])
                 ,(3,[]),(4,[])
                 ]
-- -}
-- ------------------------------------------------------------
--
-- test app

main :: IO ()
main
    = do g <- fmap read (readFile "Dependencies.txt")
         print $ rankingStd (g :: DAGList String)
