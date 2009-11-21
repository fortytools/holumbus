module Main
(
   main
)
where


import Holumbus.Distribution.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Count.DCount 
import System.Environment
import Control.Parallel.Strategies


main :: IO ()
main = do
  -- get command line arguments
  (filename : splitmapreduresult : wordsToCount : []) <- getArgs
  
  -- convert number of mappers, reducers...
  let (splitters,mappers,reducers) = read splitmapreduresult
    
  -- read the input textfile
  file <- readFile filename
  
  -- give each word a key and split list into number of splitters peaces
  -- blub2 let  splitted =  partition' (zip [0..] (words file)) [[]| _<- [1..splitters]]
  -- blub3 let  splitted =  map (:[]) . zip [0..] $  partition' (words file) [[]| _<- [1..splitters]]
  -- let  splitted =  partition' (zip [0..] $  partition' (words file) [[]| _<- [1..10000]]) ( [[]| _<- [1..splitters]])
  let  splitted =  map (:[]) (zip [0..] (partition 10000 . words $ file))
  
  -- debug
  putStrLn . show . map length $ splitted
  
  -- do the map reduce processing
  result <- client countMap countReduce (words wordsToCount) (splitters,mappers,reducers) splitted
  
  -- debug
  putStrLn . show $ result
  
  -- process the result and show it
  let result' = sum . map snd $ result
  putStrLn ("Occurence of word(s) \""++wordsToCount++"\" is " ++ (show result'))
  
  -- the end 
  return ()


partition :: Int -> [a] -> [[a]]
partition = partition2 []

partition2 :: [[a]] -> Int -> [a] -> [[a]]
partition2 lss n [] = lss
partition2 lss n ls = partition2 (left:lss) n right
  where
  (left,right) = splitAt n ls
