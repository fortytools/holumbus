
{-# OPTIONS -fglasgow-exts -fbang-patterns #-}

import Holumbus.Index.Common

import Holumbus.Index.Inverted.OneFile
import Holumbus.Index.Inverted.Memory

main :: IO ()
main = do
       m <- loadFromFile "index.bin" :: IO (Inverted)
       writeToBinFile "indexes/hayoo-index.bin" (makePersistent "indexes/hayoo-occurrences.bin" m) 
