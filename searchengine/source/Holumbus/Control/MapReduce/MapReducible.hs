{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies -XTypeSynonymInstances -XFlexibleInstances #-}

module Holumbus.Control.MapReduce.MapReducible where

class (Ord k2) => MapReducible mr k2 v2 | mr -> k2, mr -> v2 where
  mergeMR  :: mr -> mr -> IO mr
  reduceMR :: mr -> k2 -> [v2] -> IO (Maybe (mr)) 

  