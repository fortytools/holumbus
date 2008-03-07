-- ----------------------------------------------------------------------------

{- |
  Module     : Server
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The Holumbus query server for processing distributed queries.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Main where

import Network

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Time

import Control.Monad
import Control.Parallel.Strategies

import Holumbus.Index.Inverted (Inverted)
import Holumbus.Index.Common
import Holumbus.Query.Distribution.Server

data Flag = Index String 
          | Port String 
          | Verbose 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.2"

main :: IO ()
main = 
  do
  argv <- getArgs
  flags <- commandLineOpts argv

  if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
  if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

  verbose <- return (Verbose `elem` flags)
  prt <- return (getPort flags)

  idx <- return (filter isIndex flags)
  if null idx then usage ["No index file given!\n"] else return ()
  if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

  file <- return (fromIndex (head idx))

  time <- liftM toUTCTime getClockTime
  if verbose then putStrLn ("Server started at " ++ (calendarTimeToString time)) else return ()
  if verbose then putStrLn ("Loading index from " ++ file) else return ()
  i <- (loadFromFile file) :: IO Inverted
  return (rnf i)
  if verbose then putStrLn ("Listening on port " ++ (show prt)) else return ()
  listenForRequests i prt logRequest

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

fromIndex :: Flag -> FilePath
fromIndex (Index f) = f
fromIndex _ = ""

getPort :: [Flag] -> PortNumber
getPort [] = 4242
getPort ((Port p):_) = fromIntegral (read p)
getPort (_:ps) = getPort ps

usage :: [String] -> IO a
usage errs = 
  if null errs then do
  hPutStrLn stdout use
  exitWith ExitSuccess
  else do
  hPutStrLn stderr (concat errs ++ "\n" ++ use)
  exitWith (ExitFailure (-1))
    where
    header = "Server - The Holumbus query server.\n\n" ++
             "Usage: Standalone [OPTIONS]"
    use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option "i" ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option "p" ["port"] (ReqArg Port "PORT") "Listen on PORT (defaults to 4242)"
          , Option "v" ["verbose"] (NoArg Verbose) "Be more verbose"
          , Option "V" ["version"] (NoArg Version) "Output version and exit"
          , Option "?" ["help"] (NoArg Help) "Output this help and exit"
          ]
