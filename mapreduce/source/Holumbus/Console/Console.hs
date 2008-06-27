-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Console.Console
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module provides a tiny and nice implementation of a little command 
  shell. It can be feed with individual commands and provides a simple but
  powerful way to interact with your program. The following functions are
  implemented by default:
    exit - exit the console loop
    help - print a nice help

  There was a little "bug" with the System.Console.Readline package. When
  we use this option, we make a foreign call... and the Haskell library
  documentation say this about concurrency and GHC:
  
  "If you don't use the -threaded option, then the runtime does not make 
  use of multiple OS threads. Foreign calls will block all other running 
  Haskell threads until the call returns.
  The System.IO library still does multiplexing, so there can be multiple
  threads doing I/O, and this is handled internally by the runtime using 
  select."
   
  We make a foreign call, which is not in the System.IO library, so we
  have to work with -threaded when we want a fancy command history.

-}

-- ----------------------------------------------------------------------------


module Holumbus.Console.Console
(
-- * Console datatype
  ConsoleData
    
-- * Operations
, parseOption
, initializeConsole
, addConsoleCommand
, handleUserInput
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Char
import qualified Data.Map as Map

import System.IO
import System.Console.Readline



-- ----------------------------------------------------------------------------
-- datatypes
-- ----------------------------------------------------------------------------


-- | Map which contains all commands that the user can execute
type ConsoleData a = Map.Map String (ConsoleCommand a)


-- | Console command, only a pair of a function which will be executed 
--   and a description 
type ConsoleCommand a = ( Maybe (ConsoleFunction a), String )


-- | A console function. The string list represents the arguments
type ConsoleFunction a = (a -> [String] -> IO())




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


-- | The command-line prompt string.
shellString :: String
shellString = "command>"


-- | Simple "parser" for the commandline...
parseOption :: Read a => [String] -> IO (Maybe a, [String])
parseOption o
  = handle (\_ -> return (Nothing, o)) $
      do
      if ( null o ) then
          return (Nothing, o)
        else 
          return (Just $ read $ head o, tail o) 


-- | Reads the input from the stdin.
getCommandLine :: IO (String)
getCommandLine 
-- no commandline history
{-  = do
    putStr shellString
    hFlush stdout
    maybeLine <- getLine --readline shellString
    return maybeLine
-}
-- with commandline history, please use -threaded option
  = do
    maybeLine <- readline shellString
    yield
    case maybeLine of
      Nothing   -> return exitString -- EOF / control-d
      Just line -> 
        do 
          if (not $ null line) then do addHistory line else return ()
          return line
          

-- | The main loop. You know... read stdin, parse the input, execute command.
--   You can quit it by the exit-command.
handleUserInput :: ConsoleData a -> a -> IO ()
handleUserInput cdata conf
  = do
    line  <- getCommandLine
    input <- return (words line)
    cmd   <- return (command input)
    args  <- return (arguments input)
    if (cmd == exitString) 
      then do
        return ()
      else do
        if (not $ null cmd) then do handleCommand cdata conf cmd args else return ()
        handleUserInput cdata conf
    where
      command s = if (not $ null s) then head s else ""
      arguments s = tail s


-- | Picks the command an execute the command function.
handleCommand :: ConsoleData a -> a -> String -> [String] -> IO ()
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
printNoHandler :: IO ()
printNoHandler 
  = do
    putStrLn "no function handler found"


-- | Prints the "command-not-found" message.
printError :: IO ()
printError
  = do
    putStrLn "unknown command"


-- | Prints the help text.
printHelp :: ConsoleData a -> IO ()
printHelp cdata
  = do
    putStrLn "available Commands:"
    printCommands (Map.toAscList cdata)
    where
      printCommands [] = do return ()
      printCommands (x:xs) = do
                             printCommand x
                             printCommands xs
      printCommand (c, (_, t)) = do
                                 putStrLn ((prettyCommandName 15 c) ++ " -  " ++ t) 


-- | Does some pretty printing for the function descriptions
prettyCommandName :: Int -> String -> String 
prettyCommandName n s
  | n <= 0 = s
  | (n > 0) && (null s) = ' ' : prettyCommandName (n-1) s
  | otherwise           = x : prettyCommandName (n-1) xs
    where
      (x:xs) = s