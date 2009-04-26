
module Main(main) where
                 
import           Holumbus.Common.Logging
import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DFunction


main :: IO ()
main
  = do
    initializeLogging []
    initDNode $ defaultDNodeConfig ""
    addForeignDNode $ mkDNodeAddress "Master" "april" (fromInteger 7999)
    df <- newRemoteDFunction "doCounting" "Master"
    -- addForeignDNodeHandler (mkDNodeId "Master") listener
    doCounting (accessDFunction df)
    closeDFunction df
    deinitDNode


doCounting :: (Int -> IO Int) -> IO ()
doCounting f
  = do
    l <- getLine
    case  l of
      "exit" -> return ()
      _ -> do
        i <- f 1
        putStrLn $ "current Value: " ++ (show i)
        doCounting f