module Main(main) where

import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.FileSystem.FileSystem                 as FS
import qualified Holumbus.Distribution.DMapReduce               as MR
import qualified Holumbus.MapReduce.UserInterface               as UI
import qualified Holumbus.Data.KeyMap                           as KMap
import           Holumbus.MapReduce.Types

import           Examples2.Id.Id


main :: IO ()
main = do
  p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
  setPortRegistry p
  (mr,fs) <- initializeData
  UI.runUI mr ""
  deinitializeData (mr,fs)

{-
 type ActionMap = KeyMap ActionData
-}
idActionMap :: ActionMap
idActionMap = KMap.insert (readActionConfiguration idAction) KMap.empty

initializeData :: IO (MR.DMapReduce, FS.FileSystem)
initializeData 
  = do
    fs <- FS.mkFileSystemNode FS.defaultFSNodeConfig
    let actions = idActionMap
    let config  = MR.defaultMRWorkerConfig
    mr <- MR.mkMapReduceWorker fs actions config
    return (mr,fs)


deinitializeData :: (MR.DMapReduce, FS.FileSystem) -> IO ()
deinitializeData (mr,fs)
  = do
    MR.closeMapReduce mr
    FS.closeFileSystem fs
    
-- ----------------------------------------------------------------------------
