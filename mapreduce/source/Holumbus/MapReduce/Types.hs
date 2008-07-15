-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.Types
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.MapReduce.Types
(
  decodeTupleList
, encodeTupleList

, FunctionName
, FunctionDescription
, FunctionData

-- * Map-Function
, MapFunction
, BinaryMapFunction

, MapFunctionMap 
, emptyMapFunctionMap
, addMapFunctionToMap
, dispatchMapFunction
, listMapFunctions

-- * Reduce/Combine-Function
, ReduceFunction
, BinaryReduceFunction

, ReduceFunctionMap
, emptyReduceFunctionMap
, addReduceFunctionToMap
, dispatchReduceFunction
, listReduceFunctions

-- * Partition-Function
, PartitionFunction
, BinaryPartitionFunction

, PartitionFunctionMap
, emptyPartitionFunctionMap
, addPartitionFunctionToMap
, dispatchPartitionFunction
, listPartitionFunctions
)
where

import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Typeable

import qualified Holumbus.MapReduce.AccuMap as AMap




-- ----------------------------------------------------------------------------
-- general datatypes
-- ----------------------------------------------------------------------------


type FunctionName = String

type FunctionDescription = String

type FunctionData = B.ByteString
--  deriving (Show, Eq, Ord)  


-- ----------------------------------------------------------------------------
-- general encoding / decoding
-- ----------------------------------------------------------------------------

encodeTuple :: (Binary k, Binary v) => (k, v) -> B.ByteString
encodeTuple t = encode t


decodeTuple :: (Binary k, Binary v) => B.ByteString -> (k, v)
decodeTuple b = decode b


encodeTupleList :: (Binary k, Binary v) => [(k, v)] -> [B.ByteString]
encodeTupleList ls = map encodeTuple ls


decodeTupleList :: (Binary k, Binary v) => [B.ByteString] -> [(k, v)]
decodeTupleList ls = map decodeTuple ls


-- ----------------------------------------------------------------------------
-- MapTask
-- ----------------------------------------------------------------------------

-- | general MapFunction
type MapFunction k1 v1 k2 v2 = (k1 -> v1 -> IO [(k2, v2)])

-- | MapFunction on ByteStrings
type BinaryMapFunction = (B.ByteString -> IO [FunctionData])

type MapFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryMapFunction)

-- | map for storing the MapFunctions
data MapFunctionMap = MapFunctionMap (Map.Map FunctionName MapFunctionData)

instance Show MapFunctionMap where
  show _ = "MapFunctionMap"

-- | a wrapper for invoking genaral MapFunctions from ByteString from 
--   input and output  
encodeMapFunction 
  :: (Ord k2,
      Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction k1 v1 k2 v2 
  -> B.ByteString -> IO [FunctionData]
encodeMapFunction f b
  = do
    let (k, v) = decodeTuple b  
    ls <- f k v
    let sortedList = mapGroupByKey ls 
    return $ encodeTupleList sortedList


mapGroupByKey :: (Ord k2) => [(k2, v2)] -> [(k2,[v2])]
mapGroupByKey ls = AMap.toList $ AMap.fromTupleList ls 


emptyMapFunctionMap :: MapFunctionMap
emptyMapFunctionMap = MapFunctionMap Map.empty


addMapFunctionToMap
  :: (Ord k2,
      Typeable k1, Typeable v1, Typeable k2, Typeable v2, 
      Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction k1 v1 k2 v2
  -> FunctionName
  -> FunctionDescription
  -> MapFunctionMap
  -> MapFunctionMap
addMapFunctionToMap f n d (MapFunctionMap m) 
  = MapFunctionMap $ Map.insert n (n,d,t,f') m
  where
    f' = encodeMapFunction f
    t  = typeOf f


dispatchMapFunction :: MapFunctionMap -> FunctionName -> Maybe BinaryMapFunction
dispatchMapFunction (MapFunctionMap mfm) fn 
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f


listMapFunctions :: MapFunctionMap -> [(FunctionName, FunctionDescription, TypeRep)]
listMapFunctions (MapFunctionMap m) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)






-- ----------------------------------------------------------------------------
-- Combine- / ReduceTask
-- ----------------------------------------------------------------------------

type ReduceFunction k1 v1 v2 = (k1 -> [v1] -> IO (Maybe v2))

type BinaryReduceFunction = (B.ByteString -> IO (Maybe B.ByteString))

type ReduceFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryReduceFunction)

data ReduceFunctionMap = ReduceFunctionMap (Map.Map FunctionName ReduceFunctionData)

instance Show ReduceFunctionMap where
  show _ = "ReduceFunctionMap"


-- | a wrapper for invoking genaral MapFunctions from ByteString from 
--   input and output  
encodeReduceFunction
  :: (Binary k1, Binary v1, Binary v2)
  => ReduceFunction k1 v1 v2 
  -> B.ByteString -> IO (Maybe B.ByteString)
encodeReduceFunction f b
  = do
    --TODO catch exception...
    let (k, vs) = decodeTuple b   
    r <- f k vs
    case r of
      (Nothing) -> return Nothing
      (Just v)  -> return $ Just $ encodeTuple (k, v)


emptyReduceFunctionMap :: ReduceFunctionMap
emptyReduceFunctionMap = ReduceFunctionMap Map.empty


addReduceFunctionToMap
  :: (Typeable k1, Typeable v1, Typeable v2, 
     Binary k1, Binary v1, Binary v2)
  => ReduceFunction k1 v1 v2
  -> FunctionName
  -> FunctionDescription
  -> ReduceFunctionMap
  -> ReduceFunctionMap
addReduceFunctionToMap f n d (ReduceFunctionMap m) 
  = ReduceFunctionMap $ Map.insert n (n,d,t,f') m
  where
    f' = encodeReduceFunction f
    t  = typeOf f


dispatchReduceFunction :: ReduceFunctionMap -> FunctionName -> Maybe BinaryReduceFunction
dispatchReduceFunction (ReduceFunctionMap mfm) fn
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f
      

listReduceFunctions :: ReduceFunctionMap -> [(FunctionName, FunctionDescription, TypeRep)]
listReduceFunctions (ReduceFunctionMap m) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)


-- ----------------------------------------------------------------------------
-- Partition
-- ----------------------------------------------------------------------------

type PartitionFunction k1 v1 = ( [(k1,[v1])] -> IO [(k1,[v1])] )

type BinaryPartitionFunction = [B.ByteString] -> IO [B.ByteString]

type PartitionFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryPartitionFunction)

data PartitionFunctionMap = PartitionFunctionMap (Map.Map FunctionName PartitionFunctionData)

instance Show PartitionFunctionMap where
  show _ = "PartitionFunctionMap"


encodePartitionFunction
  :: (Binary k1, Binary v1)
  => PartitionFunction k1 v1 
  -> [B.ByteString] -> IO [B.ByteString]
encodePartitionFunction f ls
  = do  
    rs <- f (decodeTupleList ls)
    return $ encodeTupleList rs

emptyPartitionFunctionMap :: PartitionFunctionMap
emptyPartitionFunctionMap = PartitionFunctionMap Map.empty


addPartitionFunctionToMap
  :: (Typeable k1, Typeable v1,
     Binary k1, Binary v1)
  => PartitionFunction k1 v1
  -> FunctionName
  -> FunctionDescription
  -> PartitionFunctionMap
  -> PartitionFunctionMap
addPartitionFunctionToMap f n d (PartitionFunctionMap m) 
  = PartitionFunctionMap $ Map.insert n (n,d,t,f') m
  where
    f' = encodePartitionFunction f
    t  = typeOf f


dispatchPartitionFunction :: PartitionFunctionMap -> Maybe FunctionName -> Maybe BinaryPartitionFunction
dispatchPartitionFunction _ Nothing = Nothing
dispatchPartitionFunction (PartitionFunctionMap mfm) (Just fn)
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f
      

listPartitionFunctions :: PartitionFunctionMap -> [(FunctionName, FunctionDescription, TypeRep)]
listPartitionFunctions (PartitionFunctionMap m) = map (\(n,d,t,_)->(n,d,t)) (Map.elems m)
