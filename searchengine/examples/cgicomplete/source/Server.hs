module Main where

import Network

import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Map as M

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Control.Monad
import Control.Parallel.Strategies

import Control.Concurrent
import Control.Exception
import Control.Monad

import Codec.Compression.BZip

import Data.Binary

import System.Posix
import Network


import Holumbus.Index.Inverted.Memory (Inverted)
import Holumbus.Index.Common
import Holumbus.Index.Documents
import Holumbus.Query.Distribution.Protocol

import Holumbus.Query.Processor -- hiding (processQuery)

import Holumbus.Utility

import Holumbus.Query.Fuzzy
import Holumbus.Query.Language.Grammar
import Holumbus.Query.Distribution.Protocol
import Holumbus.Query.Result hiding (null)
import Holumbus.Query.Language.Parser

data Flag = Index String 
          | Docs String
          | Port String 
          | Help deriving (Show, Eq)

type Custom = Int

main :: IO ()
main = 
  do
  argv <- getArgs
  flags <- commandLineOpts argv

  if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

  prt <- return (getPort flags)

  idx <- return (filter isIndex flags)
  if null idx then usage ["No index file given!\n"] else return ()
  if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

  ds <- return (filter isDocs flags)
  if null ds then usage ["No documents file given!\n"] else return ()
  if length ds > 1 then usage ["Only one documents file allowed!\n"] else return ()
 
  indexFile <- return (fromIndex (head idx))
  docsFile  <- return (fromDocs  (head ds))

--  time <- liftM toUTCTime getClockTime
  i <- loadFromBinFile indexFile :: IO Inverted
  d <- loadFromBinFile docsFile  :: IO (Documents Custom)
  
  return (rnf i)
  listen i d prt


listen :: (Show (d a), Show a, Binary a, HolIndex i, HolDocuments d a) => i -> d a -> PortNumber -> IO ()
listen i d p = 
  withSocketsDo $ do
  -- Don't let the server be terminated by sockets closed unexpectedly by the client.
  installHandler sigPIPE Ignore Nothing
  idx  <- newMVar i
  docs <- newMVar d
  socket <- listenOn (PortNumber p)
  waitForRequests idx docs socket

waitForRequests :: (Show (d a), Show a, Binary a, HolIndex i, HolDocuments d a) => MVar i -> MVar (d a) -> Socket -> IO ()
waitForRequests  idx docs socket =
  do
  client <- accept socket
  forkIO $ processRequest idx docs client   -- Spawn new thread to answer the current request.
  waitForRequests idx docs socket           -- Wait for more requests.
  

processRequest ::(Show (d a), Show a, Binary a, HolIndex i, HolDocuments d a) => MVar i -> MVar (d a) -> (Handle, HostName, PortNumber) -> IO ()
processRequest i d client = 
  bracket (return client) (\(hdl, _, _) -> hClose hdl) (\cl -> processRequest' cl)
    where
    processRequest' (hdl, _, _) = 
      do
      hSetBuffering hdl NoBuffering
      -- Dispatch the request and measure the processing time.
--      s <- getCPUTime
      dispatchRequest i d hdl
--      e <- getCPUTime
      -- Call the hook provided by the user.
--      h (LogData hst prt s e spec)

-- | Dispatches a request depending on the command to the appropriate function.
dispatchRequest :: (Show (d a), Show a, Binary a, HolIndex i, HolDocuments d a) => MVar i -> MVar (d a) -> Handle -> IO ()
dispatchRequest i d hdl =
  -- Try to handle all errors.
  handle (\_ -> processFailure hdl "UNKNOWN" "processing failure") $ do   
    -- Read the header and extract the command.
    hdr <- liftM words $ hGetLine hdl
    
    -- Redirect processing depending on the command.
    res <- dispatch (head hdr) (tail hdr)
    return res
      where
      dispatch cmd hdr | cmd == queryCmd   = processQuery' i d hdl (hdr)
--                       | cmd == addCmd     = processAdd i hdl hdr
--                       | cmd == removeCmd  = processRemove i hdl hdr
--                       | cmd == replaceCmd = processReplace i hdl hdr
                       | otherwise         = processFailure hdl cmd "unknown command"   

-- | Create the configuration for the query processor.
processCfg :: ProcessConfig
processCfg = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

-- | Perform a query on a local index.
localQuery :: (HolIndex i, HolDocuments d a) => i -> d a -> Query -> IO (Result a)
localQuery i d q = return (processQuery processCfg i d q)

-- | Send a failure response.
processFailure :: Handle -> String -> String -> IO ()
processFailure hdl l m = 
  do
  hPutStrLn hdl (failureCode ++ " " ++ l ++ " " ++ m)
  return ()

getXmlResult :: Show a => Result a -> String
getXmlResult = show

-- | Process a query coming over the network.
processQuery' :: (Show (d a), Binary a, Show a, HolIndex i, HolDocuments d a) => MVar i -> MVar (d a) -> Handle -> Header -> IO ()
processQuery' i d hdl hdr =
  do
  inlen <- return (read $ head hdr)

  -- Read and decode the request.
  raw <- B.hGet hdl inlen
  q <- return (decode raw) 

  idx <- readMVar i
  docs <- readMVar d
  
  -- Process the query
  res <- localQuery idx docs q
  

  -- Encode and compress (if requested) the result.
  enc <- return (encode res)

  -- Tell the client the size of the result to expect.
  outlen <- return (B.length enc)
  
  hPutStrLn hdl (successCode ++ " " ++ (show outlen))

  -- Push the result over to the client.
  B.hPut hdl enc
  -- Indicate success
  return ()
  
isDocs :: Flag -> Bool
isDocs (Docs _) = True
isDocs _ = False

fromDocs :: Flag -> FilePath
fromDocs (Docs f) = f
fromDocs _ = ""

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

fromIndex :: Flag -> FilePath
fromIndex (Index f) = f
fromIndex _ = ""

getPort :: [Flag] -> PortNumber
getPort [] = 4242
getPort ((Port p):_) = fromIntegral (read p :: Int)
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
             "Usage: Server [OPTIONS]"
    use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option "i" ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option "d" ["documents"] (ReqArg Docs "FILE") "Load documents from FILE"
          , Option "p" ["port"] (ReqArg Port "PORT") "Listen on PORT (defaults to 4242)"
          , Option "?" ["help"] (NoArg Help) "Output this help and exit"
          ]