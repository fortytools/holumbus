module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduce
import Examples2.IdWithSimpleDMR.Id
import Data.List

main :: IO ()
main = do
  result <- simpleClient idMap idReduce () num ls
  putStrLn . show . (==ls) . sortBy sortList $ result
  where
  num = 2
  ls = zip [0..] [1..10]

sortList :: (Ord k) => (k,v) -> (k,v) -> Ordering
sortList (k1,_) (k2,_) 
  | k1 > k2 = GT
  | k1 < k2 = LT
  | otherwise = EQ