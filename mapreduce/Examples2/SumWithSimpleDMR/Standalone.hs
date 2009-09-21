module Main
where

import Control.Parallel.Strategies

main :: IO ()
main = do
  putStrLn . show $ (rnf s `seq` sum s)
  where
  s :: [Integer]
  s = [1..10^6]
