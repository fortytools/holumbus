-- ----------------------------------------------------------------------------
{- |
  Module     : 
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Main(main) where

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils                          ( handleAll )

import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.Distribution.DMapReduce               as MR
import           Holumbus.MapReduce.Types
import           Holumbus.MapReduce.MapReduce 

import           Examples.MapReduce.Grep.Grep

    
version :: String
version = "Holumbus-Distribution Standalone-Client 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handleAll (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging []
      p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
      setPortRegistry p      
      mr <- initializeData
      
      (ls,_) <- doMapReduce (grepAction) "Simpson" namesList [] 1 5 1 1 TOTRawTuple mr
      
      putStrLn "Result: "
      putStrLn $ show ls
            
      deinitializeData mr


initializeData :: IO (MR.DMapReduce)
initializeData 
  = do
    let config = MR.defaultMRClientConfig
    MR.mkMapReduceClient config


deinitializeData :: MR.DMapReduce -> IO ()
deinitializeData mr
  = do
    MR.closeMapReduce mr
    
-- ----------------------------------------------------------------------------
