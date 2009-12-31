module Crawl2.Examples2.MRIndexer
where

import           Holumbus.Distribution.SimpleDMapReduceIO
import           Holumbus.DCrawler.Core
import           Data.Maybe
import           Crawl2.Examples2.Common
import           Holumbus.DCrawler.IndexerCore
import           Control.Monad
import qualified Data.Map as M
{-
  The mapping function
  
  crawl the docs
  
  type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
  
-}
idxMap :: MapFunction ([String],[String]) Int ResultState Int ResultState
idxMap _ opts key (state, urimap)= do
  (_, state') <- runCrawler (crawlDocs . M.keys $ urimap) (crawlerConfig opts (-1)) state
  return [(key,(state',urimap))]

{-
 The reduce function
 
 build up the partial index and merge the states
 
 type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
 
-}
idxReduce :: ReduceFunction ([String],[String]) Int ResultState (ResultIndex, ResultState) -- (IndexerState Inverted Documents PlainText, ResultState)
idxReduce _ _ _ states = do
  -- merge all states together 
  let state' = mergeStates states
  -- insertRawDocWithId tuple id idx
  idx <- foldM (\idx t@(uri,_) -> insertRawDocWithId t (frommaybelookup uri (snd state')) idx) emptyIndex (cs_resultAccu . fst $ state')
  (return . Just) (idx, state')
  where
    frommaybelookup uri urimap = fromMaybe (-1) (M.lookup uri urimap)
