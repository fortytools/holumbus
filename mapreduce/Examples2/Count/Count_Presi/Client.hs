module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Count.DCount 
import System.Environment
import Control.Parallel.Strategies


main :: IO ()
main = do
  putTimeStamp "Begin Client"
  -- get command line arguments
  (filename : mapredu : term : []) <- getArgs
  
  -- convert number of mappers, reducers...
  let (mappers,reducers) = read mapredu
    
  -- read the input textfile
  file <- readFile filename
  
  -- split the input file into num of mappers parts
  let  lin = (zip [0..] . map words . lines) file;
       split = divide (length lin `div` mappers) lin
  
  -- debug
  --putStrLn . show . map length $ splitted
  
  -- do the map reduce processing
  putTimeStamp "Begin Client MR"
  result <- client countMap countReduce term (1,mappers,reducers) split
  putTimeStamp "End Client MR"
    
  -- debug
  --putStrLn . show $ result
  
  -- process the result and show it
  putTimeStamp "Begin sum result"  
  let result' = sum . map snd $ result
  putStrLn ("Occurence of word \""++term++"\" is " ++ (show result'))
  putTimeStamp "End sum result"
  putTimeStamp "End Client"  
  -- the end 
  return ()

{-
  devide image into coherent blocks
-}
divide :: Int -> [(K1,V1)] -> [[(K1,V1)]]
divide = divide' 0 []
  where
  divide' :: K1 ->  [[(K1,V1)]] -> Int -> [(K1,V1)] -> [[(K1,V1)]]
  divide' key xss n [] = xss
  divide' key xss n xs = divide' key' xss' n rest
    where
    key' = key + 1
    xss' = (xs':xss)
    (xs',rest) = splitAt n xs -- ( [(Int,[Int])] , [(Int,[Int])] )
