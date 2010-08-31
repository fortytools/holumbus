module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Count.DCount 
import System.Environment
import Control.DeepSeq


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
  let  wordlist = words file;
       splitted = devide (length wordlist `div` splitters) wordlist
  
  -- debug
  putStrLn . show . map length $ splitted

  -- write the splitted list to disk
  -- make filesystem
  fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig
  -- create the filenames and store the data to the map reduce filesystem
  let filenames = map (\i -> "input_data_"++show i) [1..(length splitted)]
  mapM_ (\(filename,ls) -> FS.createFile filename (listToByteString ls) fs) $ zip filenames splitted
  -- assign a key to each filename for distribution to splitters
  let filenames' = zip [0..] filenames
  
  -- do the map reduce processing
  putTimeStamp "Begin Client MR"
  result <- client countSplit countMap countReduce (words wordsToCount) (splitters,mappers,reducers) filenames'
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
devide :: Int -> V1 -> [[(K1,V1)]]
devide = devide' 0 []
  where
  devide' :: K1 ->  [[(K1,V1)]] -> Int -> V1 -> [[(K1,V1)]]
  devide' key xss n [] = xss
  devide' key xss n xs = devide' key' xss' n rest
    where
    key' = key + 1
    xss' = ([(key,xs')]:xss)
    (xs',rest) = splitAt n xs -- ( [(Int,[Int])] , [(Int,[Int])] )
