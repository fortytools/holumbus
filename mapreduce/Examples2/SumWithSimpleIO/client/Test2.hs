module Main where

import Holumbus.Common.FileHandling

main :: IO ()
main = writeToListFile "test2.bin" ([1..10^7]::[Integer])
