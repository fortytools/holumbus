module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.SumWithSimpleDMR.Sum
import System.Environment
import Control.Parallel.Strategies 

main :: IO ()
main = do
  ( quadrupel : [] ) <- getArgs
  let (splitters,mappers,reducers) = read quadrupel
  result <- client sumMap sumReduce () (splitters,mappers,reducers) $ map (:[]) (ls (2*mappers))
  putStrLn . show . length $ result
  putStrLn . show . sum . map snd $ result
  where
  t =  [1..10^7]
  ls num = zip [1..] $ partition' t [[] |_<- [1..num]]
