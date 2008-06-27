-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Distribution.UserInterface
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A nice console-user interface for the Holumbus-Distribution based on the
  Holumbus-Commandline-Interface. To get a standalone Distribution-Site, just
  add a distribution-object.

-}
-- ----------------------------------------------------------------------------

module Holumbus.Distribution.UserInterface
(
-- * operations
  runUI
)
where

import Control.Exception

import qualified Holumbus.Console.Console as Console
import qualified Holumbus.Distribution.Distribution as D



-- ----------------------------------------------------------------------------
-- Operations
-- ----------------------------------------------------------------------------

-- | runs the user interface... just add an fileSystem an a fancy version-number
runUI :: D.Distribution -> String -> IO ()
runUI fs version
  = do
    -- starts the console with the specified commands
    Console.handleUserInput (createConsole version) fs



-- ----------------------------------------------------------------------------
-- private functions
-- ----------------------------------------------------------------------------


createConsole :: String -> Console.ConsoleData (D.Distribution)
createConsole version =
  Console.addConsoleCommand "id" getMySiteId "get my siteId" $
  Console.addConsoleCommand "debug" printDebug "prints internal state of the filesystem (DEBUG)" $ 
  Console.addConsoleCommand "version" (printVersion version) "prints the version" $ 
  Console.initializeConsole
  

getMySiteId :: D.Distribution -> [String] -> IO ()
getMySiteId fs _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      i <- D.getMySiteId fs
      putStrLn $ show i
         
      
printDebug :: D.Distribution -> [String] -> IO ()
printDebug fs _
  = do
    handle (\e -> putStrLn $ show e) $
      do
      D.printDebug fs
  
  
printVersion :: String -> D.Distribution -> [String] -> IO ()
printVersion version _ _
  = do
    putStrLn version
