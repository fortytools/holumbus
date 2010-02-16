module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Count.DCount 
import Holumbus.MapReduce.Examples.Count.BinSplit
import System.Environment
import Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Int

instance NFData C.ByteString where
  rnf _ = ()

main :: IO ()
main = do
  putTimeStamp "Begin Client"
  -- get command line arguments
  (filename : splitmapreduresult : term : []) <- getArgs
  
  -- convert number of mappers, reducers...
  let (splitters,mappers,reducers) = read splitmapreduresult
    
  -- read the input textfile
  file <- C.readFile filename
  
  -- split the input file into num of mappers parts
  -- let splitted = devide (C.length file `div` (fromIntegral mappers)) file
  let splitted = devide 25165824 file -- split into 24MB chunks
  
  -- debug
  --putStrLn . show . map length $ splitted
  
  -- do the map reduce processing
  putTimeStamp "Begin Client MR"
  result <- client countMap countReduce term (splitters,mappers,reducers) splitted
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
  devide inputbystring into chunks of size ~n
-}
devide :: Int64 -> V1 -> [[(K1,V1)]]
devide = devide' 1 []
  where
  devide' :: K1 ->  [[(K1,V1)]] -> Int64 -> V1 -> [[(K1,V1)]]
  devide' key xss n bs = if (C.empty == bs) then xss else devide' key' xss' n bsrest
    where
    key' = key + 1
    xss' = ([(key,bs')]:xss)
    (bs',bsrest) = binsplit n bs 
