-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Distribution.Server
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The server part for processing queries in distributed manner.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Distribution.Server
  (
  -- * Server types
  Hook
  , LogData (..)
  , SpecificLogData (..)
  
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
import Holumbus.Query.Distribution.Protocol
import Holumbus.Query.Intermediate hiding (null)
import Holumbus.Query.Language
import Holumbus.Query.Processor hiding (processQuery)

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
