module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduce
import Examples2.IdWithSimpleDMR.Id

main :: IO ()
main = do
  result <- simpleClient idMap idReduce () num ls
  putStrLn . show . sum . map snd $ result
  where
  num = 2
  ls = [(x`mod`num,[0+10*x..9+10*x])|x<-[0..99]] -- == [[0..9],[10..19],[20..29],..,[990..999]]