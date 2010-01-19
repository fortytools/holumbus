module Main(main) where

import           Holumbus.Common.Logging
import           Holumbus.Network.PortRegistry.PortRegistryPort
import qualified Holumbus.Common.Debug                  as DEBUG
import           Holumbus.Common.Utils                  ( handleAll )
import Data.Binary
import Data.List
import qualified Holumbus.Console.ServerConsole                       as Console
import qualified Holumbus.FileSystem.FileSystem         as FS
import qualified Holumbus.FileSystem.Storage            as S
import           System.Environment (getArgs)
import           System.Log.Logger
import           System.Exit

version :: String
version = "Holumbus-StorageControllerDaemon 0.1"

prompt :: String
prompt = "# StorageControllerDaemon > "

localLogger :: String
localLogger = "Holumbus.StorageControllerDaemon"

pUsage :: IO ()
pUsage = do
  putStrLn "Usage: StorageControllerDaemon ConsolePort Logfile"

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
    
initialize :: IO (FS.FileSystem)
initialize = do
  p <- newPortRegistryFromXmlFile "/tmp/registry.xml"      
  setPortRegistry p
  fs <- FS.mkFileSystemController FS.defaultFSControllerConfig
  return fs

createConsole :: Console.ConsoleData (FS.FileSystem)
createConsole =
  Console.addConsoleCommand "id"        getMySiteId     "get my siteId" $
  Console.addConsoleCommand "sites"     getFileSites    "get all sites with the given file name" $
  Console.addConsoleCommand "with"      getNearestNodePortWithFile      "gets the nearest node-port with a file (DEBUG)" $
  Console.addConsoleCommand "for"       getNearestNodePortForFile       "gets the nearest node-port for a (new) file (DEBUG)" $  
  Console.addConsoleCommand "contains"  containsFile    "is file in filesystem or not" $
  Console.addConsoleCommand "create"    createFile      "adds a file" $
  Console.addConsoleCommand "append"    append          "appends to a file" $
  Console.addConsoleCommand "delete"    deleteFile      "deletes a file" $
  Console.addConsoleCommand "content"   getFileContent  "gets the content of a file" $
  Console.addConsoleCommand "data"      getFileData     "gets the metadata of a file" $
  Console.addConsoleCommand "local"     isFileLocal     "test, if the file is on the local node" $
  Console.addConsoleCommand "debug"     printDebug      "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version"   (printVersion version)          "prints the version" $ 
  Console.initializeConsole
  

getFileNameAndContent :: [String] -> (S.FileId, S.FileContent)
getFileNameAndContent []   = error "no filename given"
getFileNameAndContent (x:xs) = (x, encode $ intercalate " " xs)
-- getFileNameAndContent (x:xs) = (x, S.TextFile $ intercalate " " xs)


getFileNameAndContentSize :: [String] -> (S.FileId, Integer)
getFileNameAndContentSize []   = error "no filename given"
getFileNameAndContentSize (x1:[]) = (x1, 0)
getFileNameAndContentSize (x1:x2:_) = (x1, read x2)


getMySiteId :: FS.FileSystem -> [String] -> IO String
getMySiteId fs _
  = do
    handleAll (\e -> return $ show e) $
      do
      i <- FS.getMySiteId fs
      return $ show i
     

getFileSites :: FS.FileSystem -> [String] -> IO String
getFileSites fs opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      s <- FS.getFileSites n fs
      return $ show s


getNearestNodePortWithFile :: FS.FileSystem -> [String] -> IO String
getNearestNodePortWithFile fs opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      p <- FS.getNearestNodePortWithFile n fs
      return $ show p


getNearestNodePortForFile :: FS.FileSystem -> [String] -> IO String
getNearestNodePortForFile fs opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, s) = getFileNameAndContentSize opts
      p <- FS.getNearestNodePortForFile n s fs
      return $ show p


containsFile :: FS.FileSystem -> [String] -> IO String
containsFile fs opts 
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      b <- FS.containsFile n fs
      return $ show b


createFile :: FS.FileSystem -> [String] -> IO String
createFile fs opts 
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, c) = getFileNameAndContent opts
      FS.createFile n c fs 
      return ""


append :: FS.FileSystem -> [String] -> IO String
append fs opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, c) = getFileNameAndContent opts
      FS.appendFile n c fs
      return ""
  

deleteFile :: FS.FileSystem -> [String] -> IO String
deleteFile fs opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      FS.deleteFile n fs
      return ""


getFileContent :: FS.FileSystem -> [String] -> IO String
getFileContent fs opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      c <- FS.getFileContent n fs
      return $ show c


getFileData :: FS.FileSystem -> [String] -> IO String
getFileData fs opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      d <- FS.getFileData n fs
      return $ show d
      

isFileLocal :: FS.FileSystem -> [String] -> IO String
isFileLocal fs opts 
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      b <- FS.isFileLocal n fs
      return $ show b
    
      
printDebug :: FS.FileSystem -> [String] -> IO String
printDebug fs _
  = do
    handleAll (\e -> return . show $ e) $ DEBUG.getDebug  fs
  
  
printVersion :: String -> FS.FileSystem -> [String] -> IO String
printVersion ver _ _
  = return ver

-- ------------------------------------------------------------
