-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Console.ServerConsole
  Copyright  : Copyright (C) 2009 Sebastian Reese
  License    : MIT

  Maintainer : Sebastian Reese (str@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module provides a tiny and nice implementation of a little command 
  shell with communcation over a socket.
  
  It is basically a copy of Holumbus.Console.Console with some changes to fit network communication.

-}

-- ----------------------------------------------------------------------------
module Holumbus.Console.ServerConsole
    (
     -- * Console datatype
     ConsoleData
    
     -- * Operations
    , nextOption
    , parseOption
    , initializeConsole
    , addConsoleCommand
    , startServerConsole
    , defaultaction
    , defaultconverter
    )
where

import           Holumbus.Network.DoWithServer
import           System.IO
import           Control.Monad (forM)
import qualified Data.Map as Map
import           Holumbus.Common.Utils  ( handleAll )
import           Data.List

-- | Starts the server listening
startServerConsole ::
     ConsoleData a -- ^ the console data
  -> a             -- ^ the console config
  -> Int           -- ^ console port
  -> String        -- ^ a consoles prompt
  -> IO ()
startServerConsole cdata conf port prompt = doWithServer port (defaultaction cdata conf prompt) defaultconverter prompt

-- | This defaultimplementaion can be used if a simple INput -> Process command -> output patern is used
defaultaction :: ConsoleData a -> a -> String -> ServerAction String
defaultaction cdata conf prompt line sender clients = do
  clients' <-  handleAll (\_ -> return  $ [delete sender $ clients]) $ do
    forM (filter (==sender) clients) $
      \(Client _ handle _ _) -> do 
        result <- handleInput line cdata conf
        if result == exitString then do 
            hClose handle
            return . delete sender $ clients
          else do   
            hPutStrLn handle result
            hPutStr handle prompt
            hFlush handle
            return clients
  return . concat $ clients'

-- | default string to a converter. Converts the input lines into desired format. Here String
defaultconverter :: LineConverter String
defaultconverter = id


-- ----------------------------------------------------------------------------
-- datatypes
-- ----------------------------------------------------------------------------


-- | Map which contains all commands that the user can execute
type ConsoleData a = Map.Map String (ConsoleCommand a)


-- | Console command, only a pair of a function which will be executed 
--   and a description 
type ConsoleCommand a = ( Maybe (ConsoleFunction a), String )


-- | A console function. The string list represents the arguments
type ConsoleFunction a = (a -> [String] -> IO String )

-- ----------------------------------------------------------------------------
-- operations 
-- ----------------------------------------------------------------------------


-- | Creates a new console datatype
initializeConsole :: ConsoleData a
initializeConsole = Map.fromList [(exitString, exitCommand), (helpString, helpCommand)]


-- | Adds a new console command to the function, an existing command with the
--   same name will be overwritten
addConsoleCommand 
  :: String             -- ^ command string (the word the user has to enter when he wants to execute the command)
  -> ConsoleFunction a  -- ^ the function which should be executed
  -> String             -- ^ the function description
  -> ConsoleData a      -- ^ the old console data
  -> ConsoleData a
addConsoleCommand c f d m = Map.insert c (Just f, d) m


-- | The exit function string.
exitString  :: String
exitString = "exit"


-- | A dummy exit function (Just to print the help description, the command 
--   is handled in the main loop.
exitCommand :: ConsoleCommand a
exitCommand = ( Nothing, "exit the console")


-- | The help function string.
helpString :: String
helpString = "help"


-- | A dummy help function (Just to print the help description, the command 
--   is handled in the main loop.
helpCommand :: ConsoleCommand a
helpCommand = (Nothing, "print this help")


-- | gets the next option from the command line as string
nextOption :: [String] -> IO (Maybe String, [String])
nextOption o
  = handleAll (\_ -> return (Nothing, o)) $
      do
      if ( null o ) then
          return (Nothing, o)
        else 
          return (Just $ head o, tail o) 

-- | Simple "parser" for the commandline...
parseOption :: Read a => [String] -> IO (Maybe a, [String])
parseOption o
  = handleAll (\_ -> return (Nothing, o)) $
      do
      if ( null o ) then
          return (Nothing, o)
        else 
          return (Just $ read $ head o, tail o) 

-- | The main loop. You know... read stdin, parse the input, execute command.
--   You can quit it by the exit-command.
handleInput :: String -> ConsoleData a -> a -> IO String
handleInput line cdata conf
  = do
    input <- return (words line)
    cmd   <- return (command input)
    args  <- return (arguments input)
    if (cmd == exitString) 
      then do
        return exitString
      else do
        if (not $ null cmd) then handleCommand cdata conf cmd args else return "undefined"
    where
      command s = if (not $ null s) then head s else ""
      arguments s = tail s


-- | Picks the command an execute the command function.
handleCommand :: ConsoleData a -> a -> String -> [String] -> IO String
handleCommand cdata conf cmd args
  = do
    if (cmd == helpString)
      then do
        printHelp cdata
      else do
        handleCommand' (Map.lookup cmd cdata)
    where
        handleCommand' Nothing              = do printError
        handleCommand' (Just (Nothing, _ )) = do printNoHandler
        handleCommand' (Just (Just f, _ ))  = do f conf args


-- | Is executed when the function has no handler
printNoHandler :: IO String
printNoHandler = return "no function handler found"


-- | Prints the "command-not-found" message.
printError :: IO String
printError = return "unknown command, try help for a list of available commands"


-- | Prints the help text.
printHelp :: ConsoleData a -> IO String
printHelp cdata = return $ "available Commands:\n"++printCommands "" (Map.toAscList cdata)
    where
      printCommands acc [] = acc 
      printCommands acc (x:xs) = printCommands (acc ++ "\n" ++ printCommand x) xs
      printCommand (c, (_, t)) = (prettyCommandName 15 c) ++ " -  " ++ t


-- | Does some pretty printing for the function descriptions
prettyCommandName :: Int -> String -> String 
prettyCommandName n s
  | n <= 0 = s
  | (n > 0) && (null s) = ' ' : prettyCommandName (n-1) s
  | otherwise           = x : prettyCommandName (n-1) xs
    where
      (x:xs) = s


