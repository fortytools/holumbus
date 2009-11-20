module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.SumWithSimpleIO.Sum
import System.Environment
import Data.Time.Clock.POSIX
import Control.Parallel.Strategies
import Holumbus.Common.FileHandling

main :: IO ()
main = do
  putTimeStamp "Client Begin"
  ( triplet : [] ) <- getArgs
  let (splitters,mappers,reducers) = read triplet;
      list = partition' (ls mappers) [[]|_<-[1..splitters]]

  writeToListFile "/dev/null" list

  putTimeStamp "Client Begin MR"
  result <- client sumMap sumReduce () (splitters,mappers,reducers) list
  putTimeStamp "Client End MR"
  t2 <- getPOSIXTime
  putStrLn . show . length $ result
  putStrLn . show . sum . map snd $ result
  putTimeStamp "Client End"
  where
  t =  [1..10^7]
  ls num = let f = zip [1..] $ partition' t [[] |_<- [1..num]] in rnf f `seq` f
