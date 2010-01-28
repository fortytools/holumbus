module Main where

import System.Environment
import System.Random.Mersenne
import Data.Maybe
import Control.Parallel.Strategies

main :: IO ()
main = do
  -- read number of samples
  ( s' : [] ) <- getArgs
  let s = read s'
  
  --
  mtg <- newMTGen Nothing
  
  -- calculate the sample values
  samples <- sequence ( (parMap r0) (calcSample mtg)  [() | _ <- [1..s]])
  let samples' = catMaybes samples
  --putStrLn . show $ samples
  --putStrLn . show $ samples'

  -- reduce to inside values
  let inside = sum samples'
  putStrLn . show $ inside
  
  -- aproximate pi
  let pi = 4*inside/s
  putStrLn ("Pi is: " ++ show pi)
  
  
calcSample :: MTGen -> () -> IO (Maybe Double)
calcSample mtg _ = do
  -- make random values
  (x:y:_)::[Double] <- randoms mtg
  
  if (x*x+y*y) <= 1
    then return (Just 1)
    else return Nothing
