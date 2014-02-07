{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.DeepSeq
import           Data.List       (foldl')

main :: IO ()
main
    = do s <- readFile "l.data"
         let l' = map readList . lines $ s
         let l = id $!! l'
         print $ length l
         print $ length (filter (not . null . tail) l)

         print $ length (filter (\ (x:xs) -> all (==x) xs) $ l)
         print $ sum (map length (filter (\ (x:xs) -> all (==x) xs) $ l))

         print $ length (filter (\ (x:xs) -> any (/=x) xs) $ l)
         print $ length (filter (all (== 1))               $ l)
         print $ sum (map length (filter (all (==1)) $ l))

         t <- readFile "p.data"
         let m' = map readPair . lines $ t
         let m  = id $!! m'
         print $ length m
         print $ foldl' maxPairs (0,0) $ m
         print $ foldl' sumPairs (0,0) $ m
         return ()
    where
      readList :: String -> [Int]
      readList = read

      readPair :: String -> (Int, Int)
      readPair = read

      sumPairs (!x, !y) (!u, !v) = (x+u, y+v)
      maxPairs (!x, !y) (!u, !v) = (x `max` u, y `max` v)
      tod  = fromInteger . fromIntegral
      cp (lin', lout') = (tod lout' / tod lin') :: Double
