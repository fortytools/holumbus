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
  result <- simpleClient sumMap sumReduce () num (ls (fromIntegral num))
  putStrLn . show . length $ result
  putStrLn . show . sum . map snd $ result
  where
  ls num = zip [0..] $ partition' [0..9999999] [[] |_<- [1..num]] -- [(x`mod`num,[0+10000*x..9999+10000*x])|x<-[0..99]] -- == [[0..9999],[10000..19999],..,[y..999999]]

{-

partition'

first list, list of 
 -}
partition' :: [a] -> [[a]] -> [[a]]
partition' _   [] = []
partition' [] xss = xss
partition' (u:us) (xs:xss) = partition' us (xss ++ [xs'])
  where xs' = (u:xs)
^ ^ ^ ^ ^ ^ ^
