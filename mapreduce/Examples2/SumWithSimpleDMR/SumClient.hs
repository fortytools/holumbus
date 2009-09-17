module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduce
import Examples2.SumWithSimpleDMR.Sum
import System.Environment

main :: IO ()
main = do
  ( mappers : [] ) <- getArgs
  let num = read mappers
  result <- simpleClient sumMap sumReduce () num (ls num)
  putStrLn . show . length $ result
  putStrLn . show . sum . map snd $ result
  where
  ls num =[(x`mod`num,[0+10000*x..9999+10000*x])|x<-[0..99]] -- == [[0..9999],[10000..19999],..,[y..999999]]