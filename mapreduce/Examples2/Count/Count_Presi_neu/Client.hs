module Main
(
   main
)
where


import Holumbus.MapReduce.Examples.Count.SimpleDMapReduceIO
import Holumbus.MapReduce.Examples.Count.DCount 
import Holumbus.MapReduce.Examples.Count.BinSplit
import System.Environment
import Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Int
import Data.Time.Clock.POSIX

instance NFData C.ByteString where
  rnf _ = ()

main :: IO ()
main = do
  start <- getPOSIXTime
  putStrLn $ "start at " ++show start
  -- get command line arguments
  (filename : mapredu : term : []) <- getArgs
  
  -- convert number of mappers, reducers...
  let (mappers,reducers) = read mapredu
    
  -- read the input textfile
  file <- C.readFile filename
  
  -- split the input file into num of mappers parts
  let splitted = divide 25165824 1 file -- split into 24MB chunks
  
  -- do the map reduce processing
  result <- client countMap countReduce term (1,mappers,reducers) splitted
    
  -- process the result and show it
  let result' = sum . map snd $ result
  putStrLn ("Occurence of word \""++term++"\" is " ++ (show result'))
  end <- getPOSIXTime
  putStrLn $"End at " ++ show end
  putStrLn $"Duration: " ++show (end - start)

{-
  devide inputbystring into chunks of size ~n
-}
divide :: Int64 -> Int -> V1 -> [[(K1,V1)]]
divide n key bs
  | (C.empty == bs) = []
  | otherwise       = (result:(divide n key' bsrest))
  where
  key' = key + 1
  result = [(key,bs')]
  (bs',bsrest) = binsplit n bs
