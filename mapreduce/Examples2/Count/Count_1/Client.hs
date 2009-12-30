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
  putTimeStamp "Begin Client"
  -- get command line arguments
  (filename : splitmapreduresult : wordsToCount : []) <- getArgs
  
  -- convert number of mappers, reducers...
  let (splitters,mappers,reducers) = read splitmapreduresult
    
  -- read the input textfile
  file <- readFile filename
  
  -- split the input file into num of mappers parts
  let  splitted =  devide splitters (words file)
  
  -- debug
  putStrLn . show . map length $ splitted
  
  -- do the map reduce processing
  putTimeStamp "Begin Client MR"
  result <- client countMap countReduce (words wordsToCount) (splitters,mappers,reducers) splitted
  putTimeStamp "End Client MR"
    
  -- debug
  putStrLn . show $ result
  
  -- process the result and show it
  putTimeStamp "Begin sum result"  
  let result' = sum . map snd $ result
  putStrLn ("Occurence of word(s) \""++wordsToCount++"\" is " ++ (show result'))
  putTimeStamp "End sum result"
  putTimeStamp "End Client"  
  -- the end 
  return ()

{-
  devide image into coherent blocks
-}
devide :: Int -> [String] -> [[(Int,[String])]]
devide = devide' 0 []
  where
  devide' :: Int ->  [[(Int,[String])]] -> Int -> [String] -> [[(Int,[String])]]  
  devide' key xss n [] = xss
  devide' key xss n xs = devide' key' xss' n rest
    where
    key' = key + 1
    xss' = ([(key,xs')]:xss)
    (xs',rest) = splitAt n xs -- ( [(Int,[Int])] , [(Int,[Int])] )
