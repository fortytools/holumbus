
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
    c <- (newRemoteDMVar "counter" "Master")::IO (DMVar Int) 
    
    
    -- just take the value and leave... 
    -- in a good system, this shouldn't block the other counters
    i <- takeDMVar c
    putStrLn $ "current Value: " ++ show i ++ " -> and leaving!!!" 
    -- closeDMVar c
    -- deinitDNode
    return ()
