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

module Holumbus.MapReduce.DaemonInterface
    (
      -- * operations
      runDaemon
    )
where

import           Data.Maybe

import           Holumbus.Common.Utils                         ( handleAll )
import           Holumbus.Common.FileHandling
import qualified Holumbus.Common.Debug                          as DEBUG
import qualified Holumbus.Console.ServerConsole                       as Console

import qualified Holumbus.MapReduce.MapReduce                   as MR
import qualified Holumbus.MapReduce.Types                       as T



-- ----------------------------------------------------------------------------
-- Operations
-- ----------------------------------------------------------------------------

-- | runs the user interface... just add an fileSystem an a fancy version-number
runDaemon :: (MR.MapReduce mr) => mr -> String -> Int -> String -> IO ()
runDaemon mr version port prompt
  = do
    -- starts the console with the specified commands
    Console.startServerConsole (createConsole version) mr port prompt



-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------


createConsole :: (MR.MapReduce mr) => String -> Console.ConsoleData mr
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
  Console.initializeConsole

 
getMySiteId :: (MR.MapReduce mr) => mr -> [String] -> IO String
getMySiteId mr _
  = handleAll (\e -> return . show $ e) $
      do
      s <- MR.getMySiteId mr
      return . show $ s


getMapReduceType :: (MR.MapReduce mr) => mr -> [String] -> IO String
getMapReduceType mr _
  = handleAll (\e -> return . show $ e) $
      do
      t <- MR.getMapReduceType mr
      return . show $ t

  
startControlling :: (MR.MapReduce mr) => mr -> [String] -> IO String
startControlling mr _
  = handleAll (\e -> return . show $ e) $
      do
      MR.startControlling mr
      return "OK"


stopControlling :: (MR.MapReduce mr) => mr -> [String] -> IO String
stopControlling mr _
  = handleAll (\e -> return . show $ e) $
      do
      MR.stopControlling mr
      return "OK"


isControlling :: (MR.MapReduce mr) => mr -> [String] -> IO String
isControlling mr _
  = handleAll (\e -> return . show $ e) $
      do
      b <- MR.isControlling mr
      return . show $ b


  
doSingleStep :: (MR.MapReduce mr) => mr -> [String] -> IO String
doSingleStep mr _
  = handleAll (\e -> return . show $ e) $
      do
      MR.doSingleStep mr
      return "OK"


  
doMapReduceJob :: (MR.MapReduce mr) => mr -> [String] -> IO String
doMapReduceJob mr opts
  = do
    handleAll (\e -> return . show $ e) $
      do
      (mbName,_) <- Console.nextOption opts
      jobInfo <- (loadFromXmlFile (fromJust mbName))::IO T.JobInfo
      r <- MR.doMapReduceJob jobInfo mr
      return . (++"RESULT:\n") . show $  r


parseJob :: (MR.MapReduce mr) => mr -> [String] -> IO String
parseJob _ opts
  = do
    handleAll (\e -> return . show $ e) $
      do
      (mbName,_) <- Console.nextOption opts
      jobInfo <- (loadFromXmlFile (fromJust mbName))::IO T.JobInfo
      return . show $ jobInfo


printDebug :: (MR.MapReduce mr) => mr -> [String] -> IO String
printDebug mr _
  = do
    handleAll (\e -> return . show $ e) $ DEBUG.getDebug  mr

  
printVersion :: (MR.MapReduce mr) => String -> mr -> [String] -> IO String
printVersion version _ _
  = return version

