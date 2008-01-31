-- ----------------------------------------------------------------------------

{- |
  Module     : Server
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

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
import System.CPUTime
import System.Time

import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as IM

import Codec.Compression.BZip

import Text.Printf

import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Parallel.Strategies

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Query.Intermediate hiding (null)
import Holumbus.Index.Common
import Holumbus.Query.Processor
import Holumbus.Query.Language

data Flag = Index String 
          | Port String 
          | Verbose 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.1"

main :: IO ()
main = 
  do
  argv <- getArgs
  flags <- commandLineOpts argv

  if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
  if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

  verbose <- return (Verbose `elem` flags)
  port <- return (getPort flags)

  idx <- return (filter isIndex flags)
  if null idx then usage ["No index file given!\n"] else return ()
  if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

  startup verbose (head idx) port

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

getPort :: [Flag] -> PortNumber
getPort [] = 4242
getPort ((Port p):_) = fromIntegral (read p)
getPort (_:ps) = getPort ps

-- | Decide between hybrid and inverted and then fire up!
startup :: Bool -> Flag -> PortNumber -> IO ()
startup v (Index idxFile) p = 
  withSocketsDo $ do
  if v then putStrLn ("Loading index from " ++ idxFile) else return ()
  i <- (loadFromFile idxFile) :: IO InvIndex
  return (rnf i)
  idx <- newMVar i
  
  socket <- listenOn (PortNumber p)
  if v then putStrLn ("Listening on port " ++ (show p)) else return ()
  waitForRequests v idx socket
                              
startup _ _ _ = usage ["Internal error!\n"]

waitForRequests :: HolIndex i => Bool -> MVar i -> Socket -> IO ()
waitForRequests v idx socket = 
  do
  client <- accept socket
  forkIO $ answerRequest v idx client  -- Spawn new thread to answer the current request.
  waitForRequests v idx socket         -- Wait for more requests.

answerRequest :: HolIndex i => Bool -> MVar i -> (Handle, HostName, PortNumber) -> IO ()
answerRequest v i client = 
  bracket (return client) (\(h, _, _) -> hClose h) (\cl -> answerRequest' cl)
    where
    answerRequest' (hdl, host, port) = 
      do
      hSetBuffering hdl NoBuffering
      idx <- readMVar i

      start <- getCPUTime
      -- Read the length of what to expect.
      len <- liftM read $ hGetLine hdl
      -- Red and decode the request.
      raw <- B.hGet hdl len
      (query, c, fuzzyCfg) <- return (decode raw)
      -- Process the query
      result <- return (processPartial (ProcessConfig fuzzyCfg False) idx query)
      -- Encode and compress (if requested) the result.
      enc <- if c then return (compress . encode $ result) else return (encode result)
      -- Tell the client the size of the result to expect.
      size <- return (show $ B.length enc)
      hPutStrLn hdl size
      -- Push the result over to the client.
      B.hPut hdl enc
      end <- getCPUTime

      if v then logRequest host port query start end result size else return ()                  

logRequest :: HostName -> PortNumber -> Query -> Integer -> Integer -> Intermediate -> String -> IO ()
logRequest h p q s e r l = 
  do
  c <- getClockTime
  f <- return ((((fromIntegral e) - (fromIntegral s)) / 1000000000000) :: Float)
  putStrLn (
    (calendarTimeToString . toUTCTime $ c)
    ++ " - " ++ h ++ ":" ++ (show p)
    ++ " - " ++ (show q)
    ++ " - " ++ (printf "%.4f" f) ++ " sec"
    ++ " - " ++ (show $ IM.size r) ++ " hits"
    ++ " - " ++ l ++ " bytes"
    )

usage :: [String] -> IO a
usage errs = 
  if null errs then do
  hPutStrLn stdout use
  exitWith ExitSuccess
  else do
  hPutStrLn stderr (concat errs ++ "\n" ++ use)
  exitWith (ExitFailure (-1))
    where
    header = "Standalone - The Holumbus query server.\n\n" ++
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
