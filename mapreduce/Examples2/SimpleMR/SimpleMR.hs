module MyExamples.SimpleMR.SimpleMR
where

import qualified Holumbus.Data.AccuMap as AMap
import qualified Holumbus.Data.KeyMap as KMap

import Data.Maybe

type Map             a k1 v1 k2 v2    = a -> k1 -> v1 -> IO [(k2, v2)]
type MapPartition    a       k2 v2    = a -> Int -> [(k2,v2)] -> IO [(Int, [(k2,v2)])]
type Reduce          a       k2 v2 v3 = a -> k2 -> [v2] -> IO (Maybe v3)
type ReducePartition a       k2    v3 = a -> Int -> [(k2,v3)] -> IO [(Int, [(k2,v3)])]
type Merge           a       k2 v2    = a -> [(k2,v2)] -> IO [(k2,[v2])]

doMap :: (Ord k2, Show v1, Show k1, Show k2, Show v2) => a -> Map a k1 v1 k2 v2 -> MapPartition a k2 v2 -> Maybe Int -> (Int, [(k1,v1)]) -> IO [(Int, [(k2,v2)])]
doMap opts mapf part n (i,ls) = do

  print "List\n"
  print $ show ls ++ "\n\n"

  -- do map
  mapped_list <- mapM (\(k1, v1) -> mapf opts k1 v1) ls

  print "Mapped list\n"  
  print $ show mapped_list ++ "\n\n"
  
  -- concat the mapping results
  let tupleList = concat mapped_list

  print "Tuple list\n"  
  print $ show tupleList ++ "\n\n"
  
  -- do partition
  partedList <- case n of
    (Just n') -> part opts n' tupleList
    (Nothing) -> return [(i,tupleList)]

  print "Parted list\n"  
  print $ show partedList ++ "\n\n"

  -- return the result  
  return partedList

doReduce :: (Ord k2) => a -> Merge a k2 v2 -> Reduce a k2 v2 v3 -> ReducePartition a k2 v3 -> Maybe Int -> (Int, [(k2,v2)]) -> IO [(Int, [(k2,v3)])]
doReduce opts merge reducef part n (i,ls) = do
  -- do merge
  mergedList <- merge opts ls
  
  -- do reduce
  maybesList <- mapM (\(k2,v2) -> doReduce' opts k2 v2) mergedList
  let tupleList = catMaybes maybesList
    
  -- do partition
  partedList <- case n of
    (Just n') -> part opts n' tupleList
    (Nothing) -> return [(i,tupleList)] 
  
  -- return the result
  return partedList
  where
  doReduce' opts k2 v2
    = do
      mbV3 <- reducef opts k2 v2
      case mbV3 of
        (Nothing) -> return Nothing
        (Just v3) -> return $ Just (k2,v3)
        
-- some defaul implementations
defaultMerge :: (Ord k2) => Merge a k2 v2
defaultMerge _ ls
  = return $ AMap.toList $ AMap.fromTupleList ls
  
     