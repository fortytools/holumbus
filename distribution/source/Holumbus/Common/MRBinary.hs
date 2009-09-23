{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, TypeSynonymInstances #-}
module Holumbus.Common.MRBinary where

import Data.Binary

class MRBinary t where
  put' :: t -> Put

instance (Binary a, Binary (List a)) => MRBinary [a] where
  put' = put . List

instance Binary a => MRBinary a where
  put' = put
  
newtype  (Binary a) =>  List a = List [a]
instance (MRBinary a, Binary a) => Binary (List a) where
  put (List [])     = putWord8 0
  put (List (x:xs)) = putWord8 1 >> put' x >> put (List xs)
  get = do
    next <- getWord8
    if next==0
      then return (List [])
      else do
        x  <- get
        (List xs) <- get
        x `seq` return (List (x:xs))