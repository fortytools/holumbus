-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Distribution
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Process a Holumbus query in distributed (concurrent) manner. Note that
  this currently only works for indexes which are splitted by document.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Holumbus.Query.Distribution.Client
  (
  -- * Client types
  DistributedConfig (..)
  
  -- * Client
  , processDistributed
  , updateAdd
  , updateRemove
  , updateReplace
 
  )
where

import System.IO

import Control.Concurrent
import Control.Exception
import Control.Monad

import System.Posix
import Network

import Data.Binary
import Data.Int

import Codec.Compression.BZip

import qualified Data.ByteString.Lazy as B
import qualified Data.List as L

import Holumbus.Control.Registry
import Holumbus.Index.Common
import Holumbus.Query.Distribution.Protocol
import Holumbus.Query.Intermediate hiding (null)
import Holumbus.Query.Result (Result)
import Holumbus.Query.Language.Grammar
import Holumbus.Query.Processor hiding (processQuery)

-- | The configuration for distributed query processing.
data DistributedConfig = DistributedConfig
  { queryServers   :: ![Server]      -- ^ The list of query server to use for processing a query.
  , compressResult :: !Bool          -- ^ Indicates whether compression should be used when transmitting a result.
  , processConfig  :: !ProcessConfig -- ^ The configuration for processing a query.
  }

-- | Processes a query in distributed manner. The queries will be sent in parallel, by spawning 
-- a dedicated worker thread for each server. See also 'processQuery' and 'processPartial'.
processDistributed :: (HolDocuments d a) => DistributedConfig -> d a -> Query -> IO (Result a)
processDistributed cfg d q = 
  do
  res <- newMVar emptyIntermediate
  workers <- startWorkers (sendRequest (sendQuery res)) $ zip (queryServers cfg) (repeat request)
  waitForWorkers workers
  combined <- takeMVar res
  return (toResult d combined)
    where
    request = (q, (compressResult cfg), processConfig cfg)

-- | Updates the server's index by adding another index. Several servers can be updated at once,
-- sending the updates in parallel. A list with an error message for every server is returned.
-- If there is no message, the request was completed successfully.
updateAdd :: HolIndex i => [(Server, i)] -> IO [(Server, Maybe String)]
updateAdd srv = 
  do
  res <- newMVar []
  workers <- startWorkers (sendRequest (sendUpdate res addCmd)) input
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
  workers <- startWorkers (sendRequest (sendUpdate res removeCmd)) input
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
  workers <- startWorkers (sendRequest (sendUpdate res replaceCmd)) input
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
sendQuery :: MVar Intermediate -> (Query, Bool, ProcessConfig) -> Handle -> IO ()
sendQuery i r@(_, c, _) hdl =
  do
  enc <- return (encode r)
  -- Tell the server the type of the request and the length of the ByteString to expect.
  hPutStrLn hdl (queryCmd ++ " " ++ (show $ B.length enc))
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
