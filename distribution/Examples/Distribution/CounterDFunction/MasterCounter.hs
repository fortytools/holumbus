
module Main (main) where

import           Control.Concurrent.MVar

import           Holumbus.Common.Logging
import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DFunction


main :: IO ()
main
  = do
    initializeLogging []
    initDNode $ (defaultDNodeConfig "Master")
      { dnc_MinPort = (fromInteger 7999), dnc_MaxPort = (fromInteger 7999) }
    mv <- newMVar 0
    df <- newDFunction "doCounting" (doCounting mv) 
    doWatching mv
    closeDFunction df
    deinitDNode


doCounting :: MVar Int -> Int -> IO Int
doCounting mv i
  = do
    modifyMVar mv $ \v -> 
      do
      let v' = v + i
      return (v',v')


doWatching :: MVar Int -> IO ()
doWatching mv
  = do
    l <- getLine
    case  l of
      "exit" -> return ()
      _ -> do
        v <- readMVar mv
        putStrLn $ "current Value: " ++ (show v)
        doWatching mv