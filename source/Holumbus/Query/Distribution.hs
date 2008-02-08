-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Distribution
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Process a Holumbus query in distributed (concurrent) manner.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Holumbus.Query.Distribution 
  (
  -- * Distribution types
  DistributedConfig (..)
  , Server
  , Hook
  , LogData (..)
  , SpecificLogData (..)
  
  -- * Client
  , processDistributed
  , updateAdd
  , updateRemove
  , updateReplace
  
  -- * Server
  , listenForRequests
  , logRequest
  )
where

import System.IO
import System.CPUTime
import System.Time

import Control.Concurrent
import Control.Exception
import Control.Monad

import System.Posix
import Network

import Text.Printf

import Data.Binary
import Data.Int

import Codec.Compression.BZip

import qualified Data.ByteString.Lazy as B
import qualified Data.List as L

import Holumbus.Index.Common
import Holumbus.Query.Intermediate hiding (null)
import Holumbus.Query.Result (Result)
import Holumbus.Query.Language
import Holumbus.Query.Processor hiding (processQuery)
import Holumbus.Query.Fuzzy

-- | The configuration for distributed query processing.
data DistributedConfig = DistributedConfig
  { queryServers   :: ![Server]      -- ^ The list of query server to use for processing a query.
  , compressResult :: !Bool          -- ^ Indicates whether compression should be used when transmitting a result.
  , processConfig  :: !ProcessConfig -- ^ The configuration for processing a query.
  }

-- | General information about a request.
data LogData = LogData 
  { host     :: HostName        -- ^ The hostname of the client socket.
  , port     :: PortNumber      -- ^ The port of the client socket.
  , start    :: Integer         -- ^ Timestamp indicating the start of processing the request.
  , end      :: Integer         -- ^ Timestamp indicating the end of processing the request.
  , specific :: SpecificLogData -- ^ Specific information depending on the request type.
  }

-- | Specific information about a request.
data SpecificLogData 
  = QueryLogData             -- ^ Specific log data from a query request.
    { query  :: Query        -- ^ The incoming query recieved by the server.
    , result :: Intermediate -- ^ The partial result returned to the client.
    , size   :: Int64        -- ^ The length of the binary encoded result.
    }
  | AddLogData                        -- ^ Specific log data from a merge request.
    { addedContexts :: [Context]     -- ^ The contexts in the index.
    , addedNoWords  :: Int           -- ^ The number of words in the index.
    }
  | RemoveLogData                    -- ^ Specific log data from a remove request.
    { removedContexts :: [Context]   -- ^ The contexts in the index.
    , removedNoWords  :: Int         -- ^ The number of words in the index.
    }
  | ReplaceLogData                   -- ^ Specific log data from a replace request.
    { replacedContexts :: [Context]  -- ^ The contexts in the index.
    , replacedNoWords  :: Int        -- ^ The number of words in the index.
    }
  | FailureLogData String String     -- ^ Specific log data from a failed request, containing the command and an error message.
                      
-- | The signature of the logging hook.
type Hook = LogData -> IO ()

-- | The identification of a query server. Should take the form @hostname:port@ or just 
-- @hostname@ if the default port (4242) should be used.
type Server = String

-- | The header sent with a request (a space delimited list of strings).
type Header = [String]

-- | A global registry for worker threads.
type Workers = MVar [Worker]
-- | A single worker thread.
type Worker = MVar ()

-- | The response code indicating success.
successCode :: String
successCode = "OK"

-- | The response code indicating failure.
failureCode :: String
failureCode = "FAIL"

-- | The default port for query servers.
defaultPort :: PortNumber
defaultPort = 4242

-- | Processes a query in distributed manner. The query will be optimized before sending 
-- to the query servers. The queries will be sent in parallel, by spawning a dedicated 
-- thread for each server.
processDistributed :: (HolDocuments d) => DistributedConfig -> d -> Query -> IO Result
processDistributed cfg d q = 
  do
  res <- newMVar emptyIntermediate
  workers <- startWorkers (sendRequest (sendQuery res)) $ zip (queryServers cfg) (repeat request)
  waitForWorkers workers
  combined <- takeMVar res
  return (toResult d combined)
    where
    request = (oq, (compressResult cfg), fuzzyConfig $ processConfig cfg)
      where
      oq = if optimizeQuery $ processConfig cfg then optimize q else q

-- | Updates the server's index by adding another index. Several servers can be updated at once,
-- sending the updates in parallel. A list with an error message for every server is returned.
-- If there is no message, the request was completed successfully.
updateAdd :: HolIndex i => [(Server, i)] -> IO [(Server, Maybe String)]
updateAdd srv = 
  do
  res <- newMVar []
  workers <- startWorkers (sendRequest (sendUpdate res "ADD")) input
  waitForWorkers workers
  takeMVar res
    where
    input = map (\(s, i) -> (s, (i, s))) srv

-- | Updates the server's index by removing another index. Several servers can be updated at once,
-- sending the updates in parallel. A list with an error message for every server is returned.
-- If there is no message, the request was completed successfully.
updateRemove :: HolIndex i => [(Server, i)] -> IO [(Server, Maybe String)]
updateRemove srv = 
  do
  res <- newMVar []
  workers <- startWorkers (sendRequest (sendUpdate res "REMOVE")) input
  waitForWorkers workers
  takeMVar res
    where
    input = map (\(s, i) -> (s, (i, s))) srv

-- | Updates the server's index by replacing it with another index. Several servers can be 
-- updated at once, sending the updates in parallel. A list with an error message for every 
-- server is returned. If there is no message, the request was completed successfully.
updateReplace :: HolIndex i => [(Server, i)] -> IO [(Server, Maybe String)]
updateReplace srv = 
  do
  res <- newMVar []
  workers <- startWorkers (sendRequest (sendUpdate res "REPLACE")) input
  waitForWorkers workers
  takeMVar res
    where
    input = map (\(s, i) -> (s, (i, s))) srv

-- | Send the query to a server and merge the result with the global result.
sendRequest :: (a -> Handle -> IO ()) -> (Server, a) -> IO ()
sendRequest f (s, d) = 
  withSocketsDo $ do 
    installHandler sigPIPE Ignore Nothing
    bracket (connectTo (getHost s) (getPort s)) (hClose) (send)
    where
    send hdl = hSetBuffering hdl NoBuffering >> f d hdl

-- | Send out a query request over the specified handle.
sendQuery :: MVar Intermediate -> (Query, Bool, FuzzyConfig) -> Handle -> IO ()
sendQuery i r@(_, c, _) hdl =
  do
  enc <- return (encode r)
  -- Tell the server the type of the request and the length of the ByteString to expect.
  hPutStrLn hdl ("QUERY " ++ (show $ B.length enc))
  B.hPut hdl enc
  -- Get the length of the ByteString to expect.
  rsp <- getResponse hdl
  either (\_ -> return ()) processResponse rsp
    where
    processResponse len =
      do
      raw <- B.hGet hdl len
      -- Decode (and possibly decompress) the result
      res <- if c then return (decode . decompress $ raw) else return (decode raw)
      -- Merge with the global result.
      current <- takeMVar i
      combined <- return $! (union current res)
      putMVar i combined

-- | Send out a update request over the specified handle. The second argument contains the
-- specific command to send.
sendUpdate :: HolIndex i => MVar [(Server, Maybe String)] -> String -> (i, Server) -> Handle -> IO ()
sendUpdate r c (i, s) hdl =
  do
  enc <- return (compress . encode $ i)
  -- Tell the server the type of the request and the length of the ByteString to expect.
  hPutStrLn hdl (c ++ " " ++ (show $ B.length enc))
  handle (\_ -> return ()) (B.hPut hdl enc)
  -- Get the response
  rsp <- getResponse hdl
  either (\m -> processResponse (Just $ "FAIL " ++ m)) (\_ -> processResponse Nothing) rsp
    where
    processResponse rm = modifyMVar_ r (return . ((s, rm):))
 
-- | Read the header from the handle and figure out if the request was successful. The left value
-- represents failure and contains the error message while the right value contains the length of
-- the returned data.
getResponse :: Handle -> IO (Either String Int)
getResponse hdl =
  do
  hdr <- liftM words $ hGetLine hdl
  if null hdr then return (Left "empty header") else
    if (head hdr) == successCode then 
      if null (tail hdr) then return (Right 0) 
      else return (Right $ read $ head $ tail hdr) 
    else 
      if null (tail hdr) then return (Left "no error message") 
      else return (Left $ L.intercalate " " (tail hdr))

-- | Extract a host name from the server string.
getHost :: Server -> HostName
getHost s = takeWhile (/= ':') s

-- | Try to extract a port identifier from the server string. Fault tolerance needs to be improved.
getPort :: Server -> PortID
getPort s = let r = dropWhile (/= ':') s in
              if null r then PortNumber defaultPort else
                if null $ tail r then PortNumber defaultPort else
                  PortNumber (fromIntegral $ read $ tail r)

-- | Start an IO action for every value of a list and return the spawned workers.
startWorkers :: (a -> IO ()) -> [a] -> IO Workers
startWorkers w d = 
  do
  workers <- newMVar []            -- Initialize the list of all workers.
  foldM (startWorker w) workers d  -- Add a worker for every input value.

-- | Start a single IO action for a value and register it with the other workers.
startWorker :: (a -> IO ()) -> Workers -> a -> IO Workers
startWorker w ws d = 
  do
  worker <- newEmptyMVar                     -- Create a MVar to represent this worker.
  workers <- takeMVar ws                     -- Get the list of all workers.
  putMVar ws (worker:workers)                -- Add this worker to the list.
  forkIO ((w d) `finally` putMVar worker ()) -- Start the worker and make sure to signal success on termination.
  return ws

-- | Wait for the workers started by @startWorkers@.
waitForWorkers :: Workers -> IO ()
waitForWorkers ws = 
  do
  workers <- takeMVar ws       -- Get the list of all workers.
  case workers of
    []   -> return ()          -- All workers have finished.
    m:ms -> do
            putMVar ws ms      -- Make the list of remaining workers avaliable again.
            takeMVar m         -- Wait for the first worker from the list.
            waitForWorkers ws  -- Wait for all remaining workers.

-- | This function represents the server side. It opens a socket on the provided port and 
-- starts listening for requests. A post-processing hook is provided to allow some actions on
-- the processing results, e.g. some logging functionality.
listenForRequests :: HolIndex i => i -> PortNumber -> Hook -> IO ()
listenForRequests i p h =
  withSocketsDo $ do
  -- Don't let the server be terminated by sockets closed unexpectedly by the client.
  installHandler sigPIPE Ignore Nothing
  idx <- newMVar i
  socket <- listenOn (PortNumber p)
  waitForRequests idx socket h

waitForRequests :: HolIndex i => MVar i -> Socket -> Hook -> IO ()
waitForRequests idx socket h = 
  do
  client <- accept socket
  forkIO $ processRequest idx client h  -- Spawn new thread to answer the current request.
  waitForRequests idx socket h          -- Wait for more requests.

processRequest :: HolIndex i => MVar i -> (Handle, HostName, PortNumber) -> Hook -> IO ()
processRequest i client h = 
  bracket (return client) (\(hdl, _, _) -> hClose hdl) (\cl -> processRequest' cl)
    where
    processRequest' (hdl, hst, prt) = 
      do
      hSetBuffering hdl NoBuffering
      -- Dispatch the request and measure the processing time.
      s <- getCPUTime
      spec <- dispatchRequest i hdl
      e <- getCPUTime
      -- Call the hook provided by the user.
      h (LogData hst prt s e spec)

-- | Dispatches a request depending on the command to the appropriate function.
dispatchRequest :: HolIndex i => MVar i -> Handle -> IO SpecificLogData
dispatchRequest i hdl =
  -- Try to handle all errors.
  handle (\_ -> processFailure hdl "UNKNOWN" "processing failure") $ do   
    -- Read the header and extract the command.
    hdr <- liftM words $ hGetLine hdl
    cmd <- return (head hdr)
    -- Redirect processing depending on the command.
    res <- case cmd of
             "QUERY"   -> processQuery i hdl (tail hdr)
             "ADD"     -> processAdd i hdl (tail hdr)
             "REMOVE"  -> processRemove i hdl (tail hdr)
             "REPLACE" -> processReplace i hdl (tail hdr)
             _         -> processFailure hdl cmd "unknown command"   
    return res

-- | Send a failure response.
processFailure :: Handle -> String -> String -> IO SpecificLogData
processFailure hdl c m = 
  do
  hPutStrLn hdl (failureCode ++ " " ++ m)
  return (FailureLogData c m)

-- | Process a query coming over the network.
processQuery :: HolIndex i => MVar i -> Handle -> Header -> IO SpecificLogData
processQuery i hdl hdr =
  do
  inlen <- return (read $ head hdr)
  -- Read and decode the request.
  raw <- B.hGet hdl inlen
  (q, c, fc) <- return (decode raw)
  -- Get the index.
  idx <- readMVar i
  -- Process the query
  res <- return (processPartial (ProcessConfig fc False) idx q)
  -- Encode and compress (if requested) the result.
  enc <- if c then return (compress . encode $ res) else return (encode res)
  -- Tell the client the size of the result to expect.
  outlen <- return (B.length enc)
  hPutStrLn hdl (successCode ++ " " ++ (show outlen))
  -- Push the result over to the client.
  B.hPut hdl enc
  -- Indicate success
  return (QueryLogData q res outlen)

-- | Process a merge request.
processAdd :: HolIndex i => MVar i -> Handle -> Header -> IO SpecificLogData
processAdd i hdl hdr = 
  do
  len <- return (read $ head hdr)
  -- Read and decode the update.
  upd <- liftM (decode . decompress) $ B.hGet hdl len
  -- Merge with the current index.
  idx <- takeMVar i
  mrg <- return (mergeIndexes idx upd)
  putMVar i mrg
  -- Acknowledge the merge to the client.
  hPutStrLn hdl successCode
  -- Indicate success
  return (AddLogData (contexts mrg) (sizeWords mrg))

-- | Process a remove request.
processRemove :: HolIndex i => MVar i -> Handle -> Header -> IO SpecificLogData
processRemove i hdl hdr = 
  do
  len <- return (read $ head hdr)
  -- Read and decode the update.
  upd <- liftM (decode . decompress) $ B.hGet hdl len
  -- Merge with the current index.
  idx <- takeMVar i
  sub <- return (substractIndexes idx upd)
  putMVar i sub
  -- Acknowledge the merge to the client.
  hPutStrLn hdl successCode
  -- Indicate success
  return (RemoveLogData (contexts sub) (sizeWords sub))

-- | Process a replace request.
processReplace :: HolIndex i => MVar i -> Handle -> Header -> IO SpecificLogData
processReplace i hdl hdr =
  do
  len <- return (read $ head hdr)
  -- Read and decode the update.
  upd <- liftM (decode . decompress) $ B.hGet hdl len
  -- Replace current index.
  swapMVar i upd
  -- Acknowledge the merge to the client.
  hPutStrLn hdl successCode
  -- Indicate success
  return (ReplaceLogData (contexts upd) (sizeWords upd))

-- | A default logging function which can bee hooked into the server through @listenForRequests@.
-- The format of the printed string is fairly standard, seperating fields by \" - \".
logRequest :: Hook
logRequest (LogData h p s e r) = 
  do
  c <- getClockTime
  f <- return ((fromIntegral (e - s) / 1000000000000) :: Float)
  putStrLn (
    (calendarTimeToString . toUTCTime $ c)
    ++ " - " ++ h ++ ":" ++ (show p)
    ++ " - " ++ (logSpecific r)
    ++ " - " ++ (printf "%.4f" f) ++ " sec"
    )      

-- | Produce specific information about a request depending on the type.
logSpecific :: SpecificLogData -> String
logSpecific (QueryLogData q r s) = "QUERY - " ++
                                   (show q) ++ " - " ++ 
                                   (show $ sizeIntermediate r) ++ " hits" ++ " - " ++ 
                                   (show s) ++ " bytes" ++
                                   " - OK"
logSpecific (AddLogData c n)     = "ADD - " ++
                                   (show c) ++ " - " ++
                                   (show n) ++ " words" ++
                                   " - OK"
logSpecific (RemoveLogData c n)  = "REMOVE - " ++
                                   (show c) ++ " - " ++
                                   (show n) ++ " words" ++
                                   " - OK"
logSpecific (ReplaceLogData c n) = "REPLACE - " ++
                                   (show c) ++ " - " ++
                                   (show n) ++ " words" ++
                                   " - OK"
logSpecific (FailureLogData c m)   = c ++ " - " ++ m ++ 
                                     " - FAIL"
