module Main where


import           Holumbus.Network.PortRegistry.PortRegistryPort
import           Holumbus.MapReduce.Types
import           Holumbus.MapReduce.MapReduce
import qualified Holumbus.Distribution.DMapReduce               as MR
import           Examples2.Id.Id
import           Data.List

main :: IO ()
main = do
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p      
      mr <- initializeData
      let ls = [(y,y)|y<-[0..10]]
      (ls, _) <- doMapReduce idAction () ls [] 1 1 1 1 TOTRawTuple mr
      putStrLn . show . sortBy sortList $ ls
      deinitializeData mr

sortList :: (Ord k) => (k,v) -> (k,v) -> Ordering
sortList (k1,_) (k2,_) 
  | k1 > k2 = GT
  | k1 < k2 = LT
  | otherwise = EQ
  
initializeData :: IO (MR.DMapReduce)
initializeData 
  = do    
    let config = MR.defaultMRClientConfig
    MR.mkMapReduceClient config


deinitializeData :: MR.DMapReduce -> IO ()
deinitializeData mr
  = do
    MR.closeMapReduce mr      