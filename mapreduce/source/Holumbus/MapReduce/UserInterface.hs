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

import           Holumbus.Common.Utils
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
--  Console.addConsoleCommand "step" step "perform a single step" $
--  Console.addConsoleCommand "addJob" addJob "adds a new job" $
--  Console.addConsoleCommand "parseJob" parseJob "parses a job-xml-file and prints the result" $
--  Console.addConsoleCommand "debug" printDebug "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version" (printVersion version) "prints the version" $ 
  Console.initializeConsole

{-
step :: (MR.MapReduce mr) => mr -> [String] -> IO ()
step mr _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      MR.doSingleStep mr
      return ()
  
  
addJob :: (MR.MapReduce mr) => mr -> [String] -> IO ()
addJob mr opts
  = do
    handle (\e -> putStrLn $ show e) $
      do
      (mbName,_) <- Console.nextOption opts
      jobInfo <- (loadFromXmlFile (fromJust mbName))::IO T.JobInfo
      MR.addJob jobInfo mr
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
-}
{-
printDebug :: (MR.MapReduce mr) => mr -> [String] -> IO ()
printDebug mr _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      MR.printDebug mr
-}
  
printVersion :: (MR.MapReduce mr) => String -> mr -> [String] -> IO ()
printVersion version _ _
  = do
    putStrLn version

