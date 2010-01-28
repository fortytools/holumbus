-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.DaemonInterface
  Copyright  : Copyright (C) 2009 Sebastian Reese
  License    : MIT

  Maintainer : Sebastian Reese (str@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.MapReduce.DaemonInterfaceWithFS
    (
      -- * operations
      runDaemon
    )
where

import           Data.Maybe
import Data.Binary
import Data.List
import           Holumbus.Common.Utils                         ( handleAll )
import           Holumbus.Common.FileHandling
import qualified Holumbus.Common.Debug                          as DEBUG
import qualified Holumbus.Console.ServerConsole                       as Console

import qualified Holumbus.MapReduce.MapReduce                   as MR
import qualified Holumbus.MapReduce.Types                       as T
import qualified Holumbus.FileSystem.FileSystem                 as FS
import qualified Holumbus.FileSystem.Storage                    as S



-- ----------------------------------------------------------------------------
-- Operations
-- ----------------------------------------------------------------------------

-- | runs the user interface... just add an fileSystem an a fancy version-number
runDaemon :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> String -> Int -> String -> IO ()
runDaemon mrfs version port prompt
  = do
    -- starts the console with the specified commands
    Console.startServerConsole (createConsole version) mrfs port prompt



-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------


createConsole :: (MR.MapReduce mr) => String -> Console.ConsoleData (mr,FS.FileSystem)
createConsole version =
  Console.addConsoleCommand "site"      getMySiteId "" $
  Console.addConsoleCommand "mrtype"    getMapReduceType "" $
  Console.addConsoleCommand "startC"    startControlling "" $
  Console.addConsoleCommand "stopC"     stopControlling "" $
  Console.addConsoleCommand "isC"       isControlling "" $
  Console.addConsoleCommand "step"      doSingleStep "" $
  Console.addConsoleCommand "mrJob"     doMapReduceJob "" $
  Console.addConsoleCommand "parse"     parseJob "" $
  Console.addConsoleCommand "debug"     printDebug "" $ 
  Console.addConsoleCommand "version"   (printVersion version) "prints the version" $ 
  Console.addConsoleCommand "fsid"        getMyFSSiteId     "get my siteId" $
  Console.addConsoleCommand "sites"     getFileSites    "get all sites with the given file name" $
  Console.addConsoleCommand "with"      getNearestNodePortWithFile      "gets the nearest node-port with a file (DEBUG)" $
  Console.addConsoleCommand "for"       getNearestNodePortForFile       "gets the nearest node-port for a (new) file (DEBUG)" $  
  Console.addConsoleCommand "contains"  containsFile    "is file in filesystem or not" $
  Console.addConsoleCommand "create"    createFile      "adds a file" $
  Console.addConsoleCommand "createS"   createFiles     "adds a list of files [(Filename,FileContent)]" $
  Console.addConsoleCommand "append"    append          "appends to a file" $
  Console.addConsoleCommand "delete"    deleteFile      "deletes a file" $
  Console.addConsoleCommand "content"   getFileContent  "gets the content of a file" $
  Console.addConsoleCommand "data"      getFileData     "gets the metadata of a file" $
  Console.addConsoleCommand "local"     isFileLocal     "test, if the file is on the local node" $
  Console.addConsoleCommand "fsdebug"   printFSDebug    "prints internal state of the filesystem (DEBUG)" $ 
  Console.initializeConsole

 
getMySiteId :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getMySiteId (mr,_) _
  = handleAll (\e -> return . show $ e) $
      do
      s <- MR.getMySiteId mr
      return . show $ s


getMapReduceType :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getMapReduceType (mr,_) _
  = handleAll (\e -> return . show $ e) $
      do
      t <- MR.getMapReduceType mr
      return . show $ t

  
startControlling :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
startControlling (mr,_) _
  = handleAll (\e -> return . show $ e) $
      do
      MR.startControlling mr
      return "OK"


stopControlling :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
stopControlling (mr,_) _
  = handleAll (\e -> return . show $ e) $
      do
      MR.stopControlling mr
      return "OK"


isControlling :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
isControlling (mr,_) _
  = handleAll (\e -> return . show $ e) $
      do
      b <- MR.isControlling mr
      return . show $ b


  
doSingleStep :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
doSingleStep (mr,_) _
  = handleAll (\e -> return . show $ e) $
      do
      MR.doSingleStep mr
      return "OK"


  
doMapReduceJob :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
doMapReduceJob (mr,_) opts
  = do
    handleAll (\e -> return . show $ e) $
      do
      (mbName,_) <- Console.nextOption opts
      jobInfo <- (loadFromXmlFile (fromJust mbName))::IO T.JobInfo
      r <- MR.doMapReduceJob jobInfo mr
      return . (++"RESULT:\n") . show $  r


parseJob :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
parseJob _ opts
  = do
    handleAll (\e -> return . show $ e) $
      do
      (mbName,_) <- Console.nextOption opts
      jobInfo <- (loadFromXmlFile (fromJust mbName))::IO T.JobInfo
      return . show $ jobInfo


printDebug :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
printDebug (mr,_) _
  = do
    handleAll (\e -> return . show $ e) $ DEBUG.getDebug  mr

  
printVersion :: (MR.MapReduce mr) => String -> (mr,FS.FileSystem) -> [String] -> IO String
printVersion version _ _
  = return version

getFileNameAndContent :: [String] -> (S.FileId, S.FileContent)
getFileNameAndContent []   = error "no filename given"
getFileNameAndContent (x:xs) = (x, encode $ intercalate " " xs)
-- getFileNameAndContent (x:xs) = (x, S.TextFile $ intercalate " " xs)

getFileNamesAndContent :: [String] -> [(S.FileId, S.FileContent)]
getFileNamesAndContent [] = []
getFileNamesAndContent (_:[]) = error "no content given"
getFileNamesAndContent (fid:c:xs) = ((fid, encode c):getFileNamesAndContent xs)


getFileNameAndContentSize :: [String] -> (S.FileId, Integer)
getFileNameAndContentSize []   = error "no filename given"
getFileNameAndContentSize (x1:[]) = (x1, 0)
getFileNameAndContentSize (x1:x2:_) = (x1, read x2)


getMyFSSiteId :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getMyFSSiteId (_,fs) _
  = do
    handleAll (\e -> return $ show e) $
      do
      i <- FS.getMySiteId fs
      return $ show i
     

getFileSites :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getFileSites (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      s <- FS.getFileSites n fs
      return $ show s


getNearestNodePortWithFile :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getNearestNodePortWithFile (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      p <- FS.getNearestNodePortWithFile n fs
      return $ show p


getNearestNodePortForFile :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getNearestNodePortForFile (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, s) = getFileNameAndContentSize opts
      p <- FS.getNearestNodePortForFile n s fs
      return $ show p


containsFile :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
containsFile (_,fs) opts 
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      b <- FS.containsFile n fs
      return $ show b


createFile :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
createFile (_,fs) opts 
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, c) = getFileNameAndContent opts
      FS.createFile n c fs 
      return ""

createFiles :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
createFiles (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let l = getFileNamesAndContent opts
      FS.createFiles l fs
      return ""

append :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
append (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, c) = getFileNameAndContent opts
      FS.appendFile n c fs
      return ""
  

deleteFile :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
deleteFile (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      FS.deleteFile n fs
      return ""


getFileContent :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getFileContent (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      c <- FS.getFileContent n fs
      return $ show c


getFileData :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
getFileData (_,fs) opts
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      d <- FS.getFileData n fs
      return $ show d
      

isFileLocal :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
isFileLocal (_,fs) opts 
  = do
    handleAll (\e -> return $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      b <- FS.isFileLocal n fs
      return $ show b

printFSDebug :: (MR.MapReduce mr) => (mr,FS.FileSystem) -> [String] -> IO String
printFSDebug (_,fs) _
  = do
    handleAll (\e -> return . show $ e) $ DEBUG.getDebug  fs
