{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.List (foldl')

main :: IO ()
main
    = do s <- readFile "counts"
         let res = foldl' sumPairs (0,0) . map readPair . lines $ s
         print res
         print $ cp res
         return ()
    where
      readPair :: String -> (Int, Int)
      readPair = read
      sumPairs (!x, !y) (!u, !v) = (x+u, y+v)
      maxPairs (!x, !y) (!u, !v) = (x `max` u, y `max` v)
      tod  = fromInteger . fromIntegral
      cp (lin', lout') = (tod lout' / tod lin') :: Double
