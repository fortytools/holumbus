-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Standalone.UserInterface
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Standalone.UserInterface
(
-- * operations
  runUI
)
where

import Control.Exception

import qualified Holumbus.Console.Console as Console
import qualified Holumbus.Standalone.Standalone as S
import qualified Holumbus.MapReduce.Demo as DEMO

-- ----------------------------------------------------------------------------
-- Operations
-- ----------------------------------------------------------------------------

-- | runs the user interface... just add an fileSystem an a fancy version-number
runUI :: S.Standalone -> String -> IO ()
runUI fs version
  = do
    -- starts the console with the specified commands
    Console.handleUserInput (createConsole version) fs



-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------


createConsole :: String -> Console.ConsoleData (S.Standalone)
createConsole version =
  Console.addConsoleCommand "step" step "perform a single step" $
  Console.addConsoleCommand "addJob" addJob "adds a new job" $
  Console.addConsoleCommand "debug" printDebug "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version" (printVersion version) "prints the version" $ 
  Console.initializeConsole
  
step :: S.Standalone -> [String] -> IO ()
step s _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      S.doSingleStep s
      return ()
  
  
addJob :: S.Standalone -> [String] -> IO ()
addJob s _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      S.addJob DEMO.demoJob s
      return ()

      
printDebug :: S.Standalone -> [String] -> IO ()
printDebug s _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      S.printDebug s
  
  
printVersion :: String -> S.Standalone -> [String] -> IO ()
printVersion version _ _
  = do
    putStrLn version

