module Main
(
   main
)
where


import Examples2.SimpleMR.SimpleDMapReduce
import Examples2.IdWithSimpleDMR.Id

main :: IO ()
main = do
  result <- simpleClient idMap idReduce num ls
  putStrLn . show . sum $ result
  where
  num = 2
  ls = [(x`mod`num,[0+10*x..9+10*x])|x<-[0..99]]