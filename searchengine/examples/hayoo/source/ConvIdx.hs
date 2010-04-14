 {-# LANGUAGE BangPatterns #-} 

import Data.Foldable (foldr')

import Holumbus.Index.Common

--import Holumbus.Index.Inverted.OneFile
import Holumbus.Index.Inverted.Memory
import Holumbus.Index.Inverted.CompressedPrefixMem (InvertedOSerialized, emptyInvertedOSerialized)

main :: IO ()
main = do
       m <- loadFromFile "index.bin" :: IO (Inverted)
--       writeToBinFile "indexes/hayoo-index.bin" (makePersistent "indexes/hayoo-occurrences.bin" m) 
       writeToBinFile "index-compressed.bin" (makeCompressed m)

makeCompressed :: Inverted -> InvertedOSerialized
makeCompressed m = foldr' insert emptyInvertedOSerialized (toList m)
  where
  insert (c, w, o) i = insertOccurrences c w o i