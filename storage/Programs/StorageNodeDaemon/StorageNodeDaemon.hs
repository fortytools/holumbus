module Main(main) where

import           Holumbus.Common.Logging
import           Holumbus.Common.Utils
import           Holumbus.Network.PortRegistry.PortRegistryPort
import Holumbus.FileSystem.FileSystem
import qualified Holumbus.Console.ServerConsole as Console
import           System.Environment (getArgs)
import           System.Log.Logger
import           System.Exit

version :: String
version = "Holumbus-StorageNodeDaemon 0.1"

prompt :: String
prompt = "# StorageNodeDaemon > "

localLogger :: String
localLogger = "Holumbus.StorageNodeDaemon"

pUsage :: IO ()
pUsage = do
  putStrLn "Usage: StorageNodeDaemon ConsolePort Logfile"

params :: IO [String]
params = do
  args <- getArgs
  if length args /= 2 then do
    errorM localLogger "Wrong argument count"
    pUsage
    exitFailure
    else
      return args

main :: IO ()
main
  = handleAll (\e -> errorM localLogger $ "EXCEPTION: " ++ show e) $ do
    putStrLn ("Starting " ++ version)
    (s_cport:logfile:[]) <- params
    initializeFileLogging logfile [(localLogger, INFO),("Holumbus.Network.DoWithServer",INFO),("measure",ERROR)]
    fs <- initialize
    
    Console.startServerConsole createConsole fs (read s_cport) prompt
    
initialize :: IO (FileSystem)
initialize = do
  p <- newPortRegistryFromXmlFile "/tmp/registry.xml"      
  setPortRegistry p
  fs <- mkFileSystemNode defaultFSNodeConfig
  return fs

    
createConsole :: Console.ConsoleData FileSystem
createConsole = Console.initializeConsole
{-  Console.addConsoleCommand "register"   hdlRegister   "registers a port manually" $
  Console.addConsoleCommand "unregister" hdlUnregister "unregisters a port manually" $
  Console.addConsoleCommand "lookup"   hdlLookup     "gets the socket id for a port" $
  Console.addConsoleCommand "ports"    hdlGetPorts   "lists all ports" $ 
  Console.addConsoleCommand "version"    hdlVersion    "prints the version" $
  Console.initializeConsole -}
      

{- hdlRegister :: FileSystem -> [String] -> IO String
hdlRegister prd opts
  = do
    handleAll (\_ -> return "usage: register <streamname> <hostname> <socketId>") $
      do
      let sn = head opts
      let hn = head $ tail opts
      let po = fromInteger . read . head . tail . tail $ opts
      registerPort sn (SocketId hn po) prd
      return "Register succesfull"


hdlUnregister :: FileSystem -> [String] -> IO String
hdlUnregister prd opts
  = handleAll (\_ -> return "usage: unregister <streamname>") $
      do
      let sn = head opts
      unregisterPort sn prd
      return "Unregister succesfull"


hdlLookup :: FileSystem -> [String] -> IO String
hdlLookup prd opts
  = handleAll (\_ -> return "usage: lookup <streamname>") $
      do
      let sn = head opts
      soid <- lookupPort sn prd
      return . show $ soid


hdlGetPorts :: FileSystem -> [String] -> IO String
hdlGetPorts prd _
  = do
    ls <- getPorts prd
    return . show $ ls


hdlVersion :: FileSystem -> [String] -> IO String
hdlVersion _ _
  = return version -}

-- ------------------------------------------------------------
