module Examples2.Id.Id
where

import Holumbus.MapReduce.Types

{-
  The mapping function

  type MapFunction a k1 v1 k2 v2 = ActionEnvironment -> a -> k1 -> v1 -> IO [(k2, v2)]
  where
  
  a  = ()
  k1 = Int
  v1 = Int
  k2 = Int
  v2 = Int
-}
idMap :: MapFunction () Int Int Int Int
idMap env opts key1 value1 = return [(key1, value1)]


{-

 The reduce function

 type ReduceFunction a k2 v2 v3 = ActionEnvironment -> a -> k2 -> [v2] -> IO (Maybe v3)
 where
 a  = ()
 k2 = Int
 v2 = Int
 v3 = Int
-}
idReduce :: ReduceFunction () Int Int Int
idReduce env opts key2 values2 = return . Just . head $ values2

{- 

 The action configuration.
 
 data ActionConfiguration a k1 v1 k2 v2 v3 v4
  = ActionConfiguration {
    ac_Name          :: ActionName
  , ac_Info          :: ActionInfo
  , ac_OptEncoder    :: OptionsEncoder a
  , ac_OptDecoder    :: OptionsDecoder a
  , ac_InputEncoder  :: InputEncoder k1 v1
  , ac_OutputDecoder :: OutputDecoder k2 v4
  , ac_Split         :: Maybe (SplitConfiguration a k1 v1)
  , ac_Map           :: Maybe (MapConfiguration a k1 v1 k2 v2)
  , ac_Combine       :: Maybe (ReduceConfiguration a k2 v2 v3)
  , ac_Reduce        :: Maybe (ReduceConfiguration a k2 v3 v4)
 }
  
  where
  a  = ()
  k1 = Int
  v1 = Int
  k2 = Int
  v2 = Int
  v3 = Int
  v4 = Int
-}
idAction :: ActionConfiguration () Int Int Int Int Int Int
idAction = (defaultActionConfiguration "ID") {
     ac_Map     = Just . defaultMapConfiguration    $ idMap
   , ac_Reduce  = Just . defaultReduceConfiguration $ idReduce
  }