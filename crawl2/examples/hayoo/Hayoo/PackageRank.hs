module Hayoo.PackageRank
where

import           Control.Arrow

import           Data.List
import           Data.Map 	( Map )
import qualified Data.Map 	as M
import           Data.Maybe
import           Data.Set       ( Set )
import qualified Data.Set 	as S

-- ------------------------------------------------------------

type DAG a			= Map a (Set a)

type Ranking a			= Map a Double

-- ------------------------------------------------------------

dagFromList			:: (Ord a) => [(a, [a])] -> DAG a
dagFromList			= map (second S.fromList) >>> M.fromList

-- ------------------------------------------------------------

dagToList			:: DAG a -> [(a, [a])]
dagToList			= M.toList >>> map (second S.toList)

-- ------------------------------------------------------------

dagInvert			:: (Ord a) => DAG a -> DAG a
dagInvert			= M.foldWithKey invVs M.empty
    where
    invVs k ks acc		= S.fold invV acc1 $ ks
        where
        acc1			= M.insertWith S.union k  S.empty         $ acc		-- don't forget the roots
        invV k' acc'		= M.insertWith S.union k' (S.singleton k) $ acc'

-- ------------------------------------------------------------

ranking				:: (Ord a) => Double -> DAG a -> Ranking a
ranking w g			= r
    where
    g'				= dagInvert g
    r				= foldl insertRank M.empty $ M.keys g
        where
        insertRank r' k		= M.insert k (w * (S.fold accRank (1/w) usedBy)) r'
            where
            usedBy		= fromMaybe S.empty . M.lookup k $ g'
            accRank k' acc'	= ( fromJust . M.lookup k' $ r ) + acc'

-- ------------------------------------------------------------

d1 :: DAG Int
d1 = dagFromList [(1,[2,3])
                 ,(2,[3,4])
                 ,(3,[]),(4,[])
                 ]
