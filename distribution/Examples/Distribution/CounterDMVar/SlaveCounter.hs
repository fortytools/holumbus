
module Main(main) where
                 
import           Holumbus.Common.Logging
import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DMVar


main :: IO ()
main
  = do
    initializeLogging []
    initDNode $ defaultDNodeConfig ""
    addForeignDNode $ mkDNodeAddress "Master" "april" (fromInteger 7999)
    c <- newRemoteDMVar "counter" "Master"
    -- addForeignDNodeHandler (mkDNodeId "Master") listener
    doCounting c
    closeDMVar c
    deinitDNode


doCounting :: DMVar Int -> IO ()
doCounting c
  = do
    l <- getLine
    case  l of
      "exit" -> return ()
      _ -> do
        i <- takeDMVar c
        let i' = i+1
        putStrLn $ "old Value: " ++ (show i) ++ " -> new Value: " ++ (show i')
        putDMVar c i'
        doCounting c  