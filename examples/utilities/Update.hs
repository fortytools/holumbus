-- ----------------------------------------------------------------------------

{- |
  Module     : Update
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Update the indexes of some query servers.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import Control.Monad

import Data.Maybe

import Holumbus.Index.Common
import Holumbus.Index.Inverted
import Holumbus.Query.Distribution.Protocol
import Holumbus.Query.Distribution.Client

data Flag = Index String 
          | Server String
          | Command String
          | Version 
          | Help deriving (Show, Eq)

data Command = Add
             | Remove
             | Replace

version :: String
version = "0.1"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv
       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

       servers <- return (filter isServer flags)
       if null servers then usage ["No servers given!\n"] else return ()

       indexes <- return (filter isIndex flags)
       if null indexes then usage ["No indexes given!\n"] else return ()

       if length servers /= length indexes then usage ["One index per server required!\n"] else return ()

       command <- return (filter isCommand flags)
       if null command then usage ["No command given!\n"] else return ()
       if length command > 1 then usage ["Only one command allowed!\n"] else return ()

       cmd <- return $ getCommand (head command)
       if isNothing cmd then usage ["Unknown command!\n"] else return ()

       startup (zip (getList servers) (getList indexes)) (fromJust cmd)
       return ()

getCommand :: Flag -> Maybe Command
getCommand (Command "add") = Just Add
getCommand (Command "remove") = Just Remove
getCommand (Command "replace") = Just Replace
getCommand _ = Nothing

getList :: [Flag] -> [String]
getList [] = []
getList ((Index s):xs) = s:(getList xs)
getList ((Server s):xs) = s:(getList xs)
getList _ = []

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

isServer :: Flag -> Bool
isServer (Server _) = True
isServer _ = False

isCommand :: Flag -> Bool
isCommand (Command _) = True
isCommand _ = False

-- | Decide between commands and fire up!
startup :: [(Server, FilePath)] -> Command -> IO ()
startup sf Add = do
                 putStrLn "Updating servers using ADD..."
                 si <- mapM (\(s, f) -> liftM2 (,) (return s) (loadFromFile f :: IO Inverted)) sf
                 sr <- updateAdd si
                 printResult sr
startup sf Remove = do
                    putStrLn "Updating servers using REMOVE..."
                    si <- mapM (\(s, f) -> liftM2 (,) (return s) (loadFromFile f :: IO Inverted)) sf
                    sr <- updateRemove si
                    printResult sr
startup sf Replace = do
                     putStrLn "Updating servers using REPLACE..."
                     si <- mapM (\(s, f) -> liftM2 (,) (return s) (loadFromFile f :: IO Inverted)) sf
                     sr <- updateReplace si
                     printResult sr

printResult :: [(Server, Maybe String)] -> IO ()
printResult [] = return ()
printResult ((s, m):xs) = do
                          putStr (s ++ " : ")
                          putStrLn (fromMaybe "OK" m)
                          printResult xs

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Update - Update the indexes of query servers.\n\n" ++
           "Usage: Update [OPTIONS] where a pair of index and server has to be specified\n" ++
           "for every server to update and where COMMAND is one of the following:\n\n" ++
           "add - Add the specified index to the current index\n" ++
           "remove - Remove the contents of the specified index from the current index\n" ++
           "replace - Replace the current index by the specified index\n\n" ++
           "Avaliable options:" 
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option "i" ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option "s" ["server"] (ReqArg Server "HOST") "Update server on HOST"
          , Option "c" ["command"] (ReqArg Command "COMMAND") "Combine indexes using COMMAND"
          , Option ['V'] ["version"]  (NoArg Version)     "Output version and exit"
          , Option ['?'] ["help"]  (NoArg Help)     "Output this help and exit"
          ]
