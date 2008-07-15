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
  FunctionName
, FunctionDescription
, FunctionData(..)
, mkTupleData
, getTuple

, MapFunction
, BinaryMapFunction

, MapFunctionMap 
, emptyMapFunctionMap
, addMapFunctionToMap
, dispatchMapFunction
, listMapFunctions

, ReduceFunction
, BinaryReduceFunction

, ReduceFunctionMap
, emptyReduceFunctionMap
, addReduceFunctionToMap
, dispatchReduceFunction
, listReduceFunctions
)
where

import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Typeable




-- ----------------------------------------------------------------------------
-- general datatypes
-- ----------------------------------------------------------------------------


type FunctionName = String

type FunctionDescription = String

data FunctionData = TupleData (B.ByteString, B.ByteString)
  deriving (Show, Eq, Ord)

getTuple :: FunctionData -> (B.ByteString,  B.ByteString)
getTuple (TupleData t) = t
  
instance Binary FunctionData where
  put (TupleData (k, v)) = put k >> put v
  get
    = do
      k <- get
      v <- get
      return (TupleData (k, v))


mkTupleData :: (Binary k, Binary v) => (k, v) -> FunctionData
mkTupleData t = TupleData (encodeTupel t)

-- ----------------------------------------------------------------------------
-- general encoding / decoding
-- ----------------------------------------------------------------------------

encodeTupel :: (Binary k, Binary v) => (k, v) -> (B.ByteString, B.ByteString)
encodeTupel (k, v) = (encode k, encode v)


decodeTupel :: (Binary k, Binary v) => (B.ByteString, B.ByteString) -> (k, v)
decodeTupel (b1, b2) = (decode b1, decode b2)


encodeTupelList :: (Binary k, Binary v) => [(k, v)] -> [(B.ByteString, B.ByteString)]
encodeTupelList ls = map encodeTupel ls


decodeTupelList :: (Binary k, Binary v) => [(B.ByteString, B.ByteString)] -> [(k, v)]
decodeTupelList ls = map decodeTupel ls

encodeMaybe :: (Binary v) => Maybe v -> Maybe B.ByteString
encodeMaybe Nothing = Nothing
encodeMaybe (Just v) = Just (encode v)

decodeMaybe :: (Binary v) => Maybe B.ByteString -> Maybe v
decodeMaybe Nothing = Nothing
decodeMaybe (Just b) = Just (decode b)


-- ----------------------------------------------------------------------------
-- MapTask
-- ----------------------------------------------------------------------------

-- | general MapFunction
type MapFunction k1 v1 k2 v2 = (k1 -> v1 -> IO [(k2, v2)])

-- | MapFunction on ByteStrings
type BinaryMapFunction = (B.ByteString -> B.ByteString -> IO [FunctionData])

type MapFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryMapFunction)

-- | map for storing the MapFunctions
data MapFunctionMap = MapFunctionMap (Map.Map FunctionName MapFunctionData)

instance Show MapFunctionMap where
  show _ = "MapFunctionMap"

-- | a wrapper for invoking genaral MapFunctions from ByteString from 
--   input and output  
encodeMapFunction 
  :: (Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction k1 v1 k2 v2 
  -> B.ByteString -> B.ByteString -> IO [FunctionData]
encodeMapFunction f k1 v1
  = do
    --TODO catch exception...  
    ls <- f (decode k1) (decode v1)
    return $ map mkTupleData ls


emptyMapFunctionMap :: MapFunctionMap
emptyMapFunctionMap = MapFunctionMap Map.empty


addMapFunctionToMap
  :: (Typeable k1, Typeable v1, Typeable k2, Typeable v2, 
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

type ReduceFunction k1 v1 v2 = k1 -> [v1] -> IO (Maybe v2)

type BinaryReduceFunction = B.ByteString -> [B.ByteString] -> IO (Maybe B.ByteString)

type ReduceFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryReduceFunction)

data ReduceFunctionMap = ReduceFunctionMap (Map.Map FunctionName ReduceFunctionData)

instance Show ReduceFunctionMap where
  show _ = "ReduceFunctionMap"


-- | a wrapper for invoking genaral MapFunctions from ByteString from 
--   input and output  
encodeReduceFunction
  :: (Binary k1, Binary v1, Binary v2)
  => ReduceFunction k1 v1 v2 
  -> B.ByteString -> [B.ByteString] -> IO (Maybe B.ByteString)
encodeReduceFunction f k1 v1
  = do
    --TODO catch exception...  
    r <- f (decode k1) (map decode v1)
    return $ encodeMaybe r


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
