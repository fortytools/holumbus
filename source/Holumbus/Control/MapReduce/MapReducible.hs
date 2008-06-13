{-# OPTIONS_GHC -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleInstances #-}

module Holumbus.Control.MapReduce.MapReducible where

import Data.List
import Holumbus.Index.Common
import Holumbus.Index.Inverted

class (Ord k2) => MapReducible mr k2 v2 where
  mergeMR  :: mr -> mr -> mr
  reduceMR :: mr -> k2 -> [v2] -> IO (Maybe (mr)) 
  
instance MapReducible Inverted Context (Word, DocId, Position) where
  mergeMR       = mergeIndexes
  reduceMR _ c os = return $ Just $ foldl' (\i (w, d, p) -> insertPosition c w d p i) emptyInverted os 
  
  
  

{-
class MapReducable mr k1 v1 k2 v2 where
  mAP     :: k1 -> v1   -> IO [(k2, v2)]
  rEDUCE  :: k2 -> [v2] -> IO (Maybe mr)

instance MapReducable Inverted DocId URI String (String, String, DocId, Position)  where
  mAP    = undefined
  rEDUCE = undefined
-}        
  