module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Examples2.CountWithSimpleIO.DCount
import System.Environment
import Control.Parallel.Strategies


main :: IO ()
main = do 
  (filename : mappers : wordsToCount : []) <- getArgs
  file <- readFile filename
  let num = read mappers
  let  prepared =  prepare $ rnf file `seq` partition file num
  putStrLn . show . map (length .snd) $ prepared
  result <- client countMap countReduce (words wordsToCount) num prepared
  let result' = sum . map snd $ result
  putStrLn ("Occurence of word(s) \""++wordsToCount++"\"is " ++ (show result'))
  return ()

prepare :: [[String]] -> [(Int,[String])]
prepare = zip [0..]

partition :: String -> Int -> [[String]]
partition s i = partition' (words s) [[]|_<-[1..i]]

{-

partition'

first list, list of 
 -}
partition' :: [a] -> [[a]] -> [[a]]
partition' _   [] = []
partition' [] xss = xss
partition' (u:us) (xs:xss) = partition' us (xss ++ [xs'])
  where xs' = (u:xs)
