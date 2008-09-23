-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.MapReduce.UserInterface
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.MapReduce.UserInterface
(
-- * operations
  runUI
)
where

import           Control.Exception
import           Data.Maybe

import           Holumbus.Common.FileHandling
import qualified Holumbus.Common.Debug as DEBUG
import qualified Holumbus.Console.Console as Console
import qualified Holumbus.MapReduce.MapReduce as MR
import qualified Holumbus.MapReduce.Types as T



-- ----------------------------------------------------------------------------
-- Operations
-- ----------------------------------------------------------------------------

-- | runs the user interface... just add an fileSystem an a fancy version-number
runUI :: (MR.MapReduce mr) => mr -> String -> IO ()
runUI mr version
  = do
    -- starts the console with the specified commands
    Console.handleUserInput (createConsole version) mr



-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------


createConsole :: (MR.MapReduce mr) => String -> Console.ConsoleData mr
createConsole version =
  Console.addConsoleCommand "site" getMySiteId "" $
  Console.addConsoleCommand "mrtype" getMapReduceType "" $
  Console.addConsoleCommand "startC" startControlling "" $
  Console.addConsoleCommand "stopC" stopControlling "" $
  Console.addConsoleCommand "isC" isControlling "" $
  Console.addConsoleCommand "step" doSingleStep "" $
  Console.addConsoleCommand "mrJob" doMapReduceJob "" $
  Console.addConsoleCommand "parse" parseJob "" $
  Console.addConsoleCommand "debug" printDebug "" $ 
  Console.addConsoleCommand "version" (printVersion version) "prints the version" $ 
  Console.initializeConsole

 
getMySiteId :: (MR.MapReduce mr) => mr -> [String] -> IO ()
getMySiteId mr _
  = handle (\e -> putStrLn $ show e) $
      do
      s <- MR.getMySiteId mr
      putStrLn $ show s


getMapReduceType :: (MR.MapReduce mr) => mr -> [String] -> IO ()
getMapReduceType mr _
  = handle (\e -> putStrLn $ show e) $
      do
      t <- MR.getMapReduceType mr
      putStrLn $ show t

  
startControlling :: (MR.MapReduce mr) => mr -> [String] -> IO ()
startControlling mr _
  = handle (\e -> putStrLn $ show e) $
      do
      MR.startControlling mr


stopControlling :: (MR.MapReduce mr) => mr -> [String] -> IO ()
stopControlling mr _
  = handle (\e -> putStrLn $ show e) $
      do
      MR.stopControlling mr


isControlling :: (MR.MapReduce mr) => mr -> [String] -> IO ()
isControlling mr _
  = handle (\e -> putStrLn $ show e) $
      do
      b <- MR.isControlling mr
      putStrLn $ show b


  
doSingleStep :: (MR.MapReduce mr) => mr -> [String] -> IO ()
doSingleStep mr _
  = handle (\e -> putStrLn $ show e) $
      do
      MR.doSingleStep mr


  
doMapReduceJob :: (MR.MapReduce mr) => mr -> [String] -> IO ()
doMapReduceJob mr opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      (mbName,_) <- Console.nextOption opts
      jobInfo <- (loadFromXmlFile (fromJust mbName))::IO T.JobInfo
      r <- MR.doMapReduceJob jobInfo mr
      putStrLn "RESULT:" 
      putStrLn $ show r
      return ()


parseJob :: (MR.MapReduce mr) => mr -> [String] -> IO ()
parseJob _ opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      (mbName,_) <- Console.nextOption opts
      jobInfo <- (loadFromXmlFile (fromJust mbName))::IO T.JobInfo
      putStrLn $ show jobInfo
      return ()


printDebug :: (MR.MapReduce mr) => mr -> [String] -> IO ()
printDebug mr _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      DEBUG.printDebug mr

  
printVersion :: (MR.MapReduce mr) => String -> mr -> [String] -> IO ()
printVersion version _ _
  = do
    putStrLn version

