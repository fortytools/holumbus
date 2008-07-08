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

, MapFunction
, BinaryMapFunction

, MapFunctionMap 
, emptyMapFunctionMap
, addMapFunctionToMap
, dispatchMapFunction


, ReduceFunction
, BinaryReduceFunction

, ReduceFunctionMap
, emptyReduceFunctionMap
, addReduceFunctionToMap
, dispatchReduceFunction

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
type BinaryMapFunction = (B.ByteString -> B.ByteString -> IO [(B.ByteString, B.ByteString)])

type MapFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryMapFunction)

-- | map for storing the MapFunctions
type MapFunctionMap = Map.Map FunctionName MapFunctionData

-- | a wrapper for invoking genaral MapFunctions from ByteString from 
--   input and output  
encodeMapFunction 
  :: (Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction k1 v1 k2 v2 
  -> B.ByteString -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
encodeMapFunction f k1 v1
  = do
    --TODO catch exception...  
    ls <- f (decode k1) (decode v1)
    return $ encodeTupelList ls


emptyMapFunctionMap :: MapFunctionMap
emptyMapFunctionMap = Map.empty


addMapFunctionToMap
  :: (Typeable k1, Typeable v1, Typeable k2, Typeable v2, 
      Binary k1, Binary v1, Binary k2, Binary v2)
  => MapFunction k1 v1 k2 v2
  -> FunctionName
  -> FunctionDescription
  -> MapFunctionMap
  -> MapFunctionMap
addMapFunctionToMap f n d m 
  = Map.insert n (n,d,t,f') m
  where
    f' = encodeMapFunction f
    t  = typeOf f


dispatchMapFunction :: MapFunctionMap -> FunctionName -> Maybe BinaryMapFunction
dispatchMapFunction mfm fn 
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f





      -- ----------------------------------------------------------------------------
-- Combine- / ReduceTask
-- ----------------------------------------------------------------------------

type ReduceFunction k1 v1 v2 = k1 -> [v1] -> IO (Maybe v2)

type BinaryReduceFunction = B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)

type ReduceFunctionData = (FunctionName, FunctionDescription, TypeRep, BinaryReduceFunction)

type ReduceFunctionMap = Map.Map FunctionName ReduceFunctionData


-- | a wrapper for invoking genaral MapFunctions from ByteString from 
--   input and output  
encodeReduceFunction
  :: (Binary k1, Binary v1, Binary v2)
  => ReduceFunction k1 v1 v2 
  -> B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
encodeReduceFunction f k1 v1
  = do
    --TODO catch exception...  
    r <- f (decode k1) (decode v1)
    return $ encodeMaybe r


emptyReduceFunctionMap :: ReduceFunctionMap
emptyReduceFunctionMap = Map.empty


addReduceFunctionToMap
  :: (Typeable k1, Typeable v1, Typeable v2, 
     Binary k1, Binary v1, Binary v2)
  => ReduceFunction k1 v1 v2
  -> FunctionName
  -> FunctionDescription
  -> ReduceFunctionMap
  -> ReduceFunctionMap
addReduceFunctionToMap f n d m 
  = Map.insert n (n,d,t,f') m
  where
    f' = encodeReduceFunction f
    t  = typeOf f


dispatchReduceFunction :: ReduceFunctionMap -> FunctionName -> Maybe BinaryReduceFunction
dispatchReduceFunction mfm fn
  = check $ Map.lookup fn mfm
    where
      check (Nothing) = Nothing
      check (Just (_,_,_,f)) = Just f
      