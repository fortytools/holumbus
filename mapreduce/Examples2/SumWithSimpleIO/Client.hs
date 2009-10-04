module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.SumWithSimpleIO.Sum
import System.Environment

main :: IO ()
main = do
  ( triplet : [] ) <- getArgs
  let (splitters,mappers,reducers) = read triplet
  result <- client sumMap sumReduce () (splitters,mappers,reducers) $ partition' (ls mappers) [[]|_<-[1..splitters]]
  putStrLn . show . length $ result
  putStrLn . show . sum . map snd $ result
  where
  t =  [1..10^7]
  ls num = zip [1..] $ partition' t [[] |_<- [1..num]]
