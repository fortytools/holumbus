-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.FileSystem.UserInterface 
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A nice console-user interface for the Holumbus-Filesystem based on the
  Holumbus-Commandline-Interface. To get a standalone FileSystem-Site, just
  add a filesystem-object.

-}
-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.UserInterface
(
-- * operations
  runUI
)
where

import Control.Exception

import Data.List

import qualified Holumbus.Console.Console as Console
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.FileSystem.Storage as S



-- ----------------------------------------------------------------------------
-- Operations
-- ----------------------------------------------------------------------------

-- | runs the user interface... just add an fileSystem an a fancy version-number
runUI :: FS.FileSystem -> String -> IO ()
runUI fs version
  = do
    -- starts the console with the specified commands
    Console.handleUserInput (createConsole version) fs



-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------


createConsole :: String -> Console.ConsoleData (FS.FileSystem)
createConsole version =
  Console.addConsoleCommand "id" getMySiteId "get my siteId" $
  Console.addConsoleCommand "sites" getFileSites "get all sites with the given file name" $
  Console.addConsoleCommand "with" getNearestNodePortWithFile "gets the nearest node-port with a file (DEBUG)" $
  Console.addConsoleCommand "for" getNearestNodePortForFile "gets the nearest node-port for a (new) file (DEBUG)" $  
  Console.addConsoleCommand "contains" containsFile "is file in filesystem or not" $
  Console.addConsoleCommand "create" createFile "adds a file" $
  Console.addConsoleCommand "append" append "appends to a file" $
  Console.addConsoleCommand "delete" deleteFile "deletes a file" $
  Console.addConsoleCommand "content" getFileContent "gets the content of a file" $
  Console.addConsoleCommand "data" getFileData "gets the metadata of a file" $
  Console.addConsoleCommand "local" isFileLocal "test, if the file is on the local node" $
  Console.addConsoleCommand "debug" printDebug "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version" (printVersion version) "prints the version" $ 
  Console.initializeConsole
  

getFileNameAndContent :: [String] -> (S.FileId, S.FileContent)
getFileNameAndContent []   = error "no filename given"
getFileNameAndContent (x:xs) = (x, S.TextFile $ intercalate " " xs)


getFileNameAndContentSize :: [String] -> (S.FileId, Integer)
getFileNameAndContentSize []   = error "no filename given"
getFileNameAndContentSize (x1:[]) = (x1, 0)
getFileNameAndContentSize (x1:x2:_) = (x1, read x2)


getMySiteId :: FS.FileSystem -> [String] -> IO ()
getMySiteId fs _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      i <- FS.getMySiteId fs
      putStrLn $ show i
     

getFileSites :: FS.FileSystem -> [String] -> IO ()
getFileSites fs opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      s <- FS.getFileSites n fs
      putStrLn $ show s


getNearestNodePortWithFile :: FS.FileSystem -> [String] -> IO ()
getNearestNodePortWithFile fs opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      p <- FS.getNearestNodePortWithFile n fs
      putStrLn $ show p


getNearestNodePortForFile :: FS.FileSystem -> [String] -> IO ()
getNearestNodePortForFile fs opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, s) = getFileNameAndContentSize opts
      p <- FS.getNearestNodePortForFile n s fs
      putStrLn $ show p


containsFile :: FS.FileSystem -> [String] -> IO ()
containsFile fs opts 
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      b <- FS.containsFile n fs
      putStrLn $ show b


createFile :: FS.FileSystem -> [String] -> IO ()
createFile fs opts 
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, c) = getFileNameAndContent opts
      FS.createFile n c fs 


append :: FS.FileSystem -> [String] -> IO ()
append fs opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, c) = getFileNameAndContent opts
      FS.appendFile n c fs
  

deleteFile :: FS.FileSystem -> [String] -> IO ()
deleteFile fs opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      FS.deleteFile n fs


getFileContent :: FS.FileSystem -> [String] -> IO ()
getFileContent fs opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      c <- FS.getFileContent n fs
      putStrLn $ show c


getFileData :: FS.FileSystem -> [String] -> IO ()
getFileData fs opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      d <- FS.getFileData n fs
      putStrLn $ show d
      

isFileLocal :: FS.FileSystem -> [String] -> IO ()
isFileLocal fs opts 
  = do
    handle (\e -> putStrLn $ show e) $
      do
      let (n, _) = getFileNameAndContent opts
      b <- FS.isFileLocal n fs
      putStrLn $ show b
    
      
printDebug :: FS.FileSystem -> [String] -> IO ()
printDebug fs _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      FS.printDebug fs
  
  
printVersion :: String -> FS.FileSystem -> [String] -> IO ()
printVersion version _ _
  = do
    putStrLn version
