
module Main (main) where

import           Holumbus.Common.Logging
import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DMVar


main :: IO ()
main
  = do
    initializeLogging []
    initDNode $ (defaultDNodeConfig "Master")
      { dnc_MinPort = (fromInteger 7999), dnc_MaxPort = (fromInteger 7999) }
    c <- newDMVar "counter" 0
    doWatching c
    closeDMVar c
    deinitDNode


doWatching :: DMVar Int -> IO ()
doWatching c
  = do
    l <- getLine
    case  l of
      "exit" -> return ()
      _ -> do
        i <- readDMVar c
        putStrLn $ "current Value: " ++ (show i)
        doWatching c