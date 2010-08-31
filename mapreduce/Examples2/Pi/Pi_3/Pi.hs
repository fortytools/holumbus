module Main where

import System.Environment
import System.Random.Mersenne
import qualified Data.List as L
import Control.Parallel
import Control.DeepSeq
import GHC.Int

main :: IO ()
main = do
  -- read number of samples
  ( s : rnd : _ ) <- getArgs

  --
  --let seed = Just (read rnd)
  mtg <- newMTGen Nothing
  rands <- randoms mtg

  -- map/calculate the sample values
  let samples = mapP (read s) calcSample rands
      -- reduce to inside values
      inside = fromIntegral . foldl (\n b -> if b then (n+1) else n) 0 $ samples

  -- aproximate pi
  let pi = 4*inside/(read s)
  putStrLn ("Pi is ~" ++ show pi)
  

calcSample :: Double -> Double -> Bool
calcSample x y = (x*x+y*y) <= 1

mapP :: Int64 -> (Double -> Double -> Bool) -> [Double] -> [Bool] 
mapP 0 _ _ =  []
mapP n f (x1:y1:xs) = (f1: (mapP (n-1) f xs) )
  where
  f1 = f x1 y1
