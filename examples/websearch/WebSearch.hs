-- ----------------------------------------------------------------------------

{- |
  Module     : WebSearch
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  An example of how Holumbus can be used together with the Janus application
  server to create a web service.
 
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.WebSearch where

import Text.XML.HXT.Arrow

import Holumbus.Index.Common (contexts)
import Holumbus.Index.Inverted
import Holumbus.Query.Parser
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Query.Ranking

import Network.Server.Janus.Core
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

import Control.Concurrent  -- For the global MVar

import Network.CGI         -- For decoding URI-encoded strings

-- Status information of query processing.
type StatusResult = (Status, VerboseResult)
type Status = (String, Int, Float, Int, Float)

websearchShader :: ShaderCreator
websearchShader = mkDynamicCreator $ proc (_, _) -> do
  tmp <- arrIO $ loadFromFile -< "indexes/vl.xml" -- Should be configurable (from Context)
  mix <- arrIO $ newMVar -< tmp
  returnA -< websearchService mix

websearchService :: MVar InvIndex -> Shader
websearchService mix = proc inTxn -> do
  status      <- constA $ defaultStatus                                 -< ()
  idx         <- arrIO $ readMVar                                       -< mix
  request     <- getValDef (_transaction_http_request_cgi_ "@query") "" -< inTxn
  outTxn      <- setVal _transaction_http_response_mime "text/xml"      -<< inTxn
  parseResult <- arr $ parseQuery                                       -< urlDecode request
  if Prelude.null parseResult then do
    status    <- arr $ setMessage "Null query"        -< status
    xmlStatus <- (pickleStatusResult >>> writeString) -< (status, emptyVerboseResult)
    setVal _transaction_http_response_body xmlStatus  -<< outTxn    
    else do  
      if (snd . head) parseResult == "" then do
        query         <- arr $ (fst . head)                          -< parseResult
        result        <- arr $ (\(q, i) -> process q i (contexts i)) -< (query, idx)
        rankedResult  <- arr $ rank                                  -< result
        verboseResult <- arr $ annotateResult idx                    -<< rankedResult
        status        <- arr $ (setResult rankedResult)              -<< status
        xmlStatus     <- (pickleStatusResult >>> writeString)        -< (status, verboseResult)
        setVal _transaction_http_response_body xmlStatus             -<< outTxn   
        else do
          status    <- arr $ setMessage (parseError parseResult) -<< status
          xmlStatus <- (pickleStatusResult >>> writeString)       -< (status, emptyVerboseResult)
          setVal _transaction_http_response_body xmlStatus        -<< outTxn   
  where
    writeString = writeDocumentToString [ (a_indent, v_1), (a_output_encoding, isoLatin1) ]
    pickleStatusResult = xpickleVal xpStatusResult
    parseError pr = "Could not parse query: " ++ ((snd . head) pr)

defaultStatus :: Status
defaultStatus = ("", 0, 0.0, 0, 0.0)

xpStatusResult :: PU StatusResult
xpStatusResult = xpElem "holumbus" (xpPair xpStatus xpVerboseResult)

xpStatus :: PU Status
xpStatus = xpElem "status" (xp5Tuple (xpElem "message" xpPrim)
           (xpElem "doccount" xpPrim) (xpElem "docscore" xpPrim)
           (xpElem "wordcount" xpPrim) (xpElem "wordscore" xpPrim))

setResult :: Result -> Status -> Status
setResult r (m, _, _, _, _) = (m, sizeDocs r, maxScoreDocs r, sizeWords r, maxScoreWords r)

setMessage :: String -> Status -> Status
setMessage m (_, dh, ds, wh, ws) = (m, dh, ds, wh, ws)