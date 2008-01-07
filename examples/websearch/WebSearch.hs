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

import Holumbus.Index.Common
import Holumbus.Index.Inverted
import Holumbus.Query.Parser
import Holumbus.Query.Processor
import Holumbus.Query.Result

import Network.Server.Janus.Core
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

import Control.Concurrent


websearchShader :: ShaderCreator
websearchShader = mkDynamicCreator $ proc (_, _) -> do
  tmp <- arrIO $ loadFromFile -< "indexes/sd.xml" -- Should be configurable (from Context)
  mix <- arrIO $ newMVar -< tmp
  returnA -< websearchService mix

websearchService :: MVar InvIndex -> Shader
websearchService mix = proc inTxn -> do
  idx <- arrIO $ readMVar -< mix
  request <- getValDef (_transaction_http_request_cgi_ "@query") "" -< inTxn
  parseResult <- arr $ parseQuery -< request
  query <- arr $ (fst . head) -< parseResult  -- Some error checking needed
  result <- arr $ (\(q, i) -> process q i (contexts i)) -< (query, idx)
  xmlResult <- (pickleResult >>> writeDocumentToString [ (a_indent, v_1) ]) -< result
  setVal _transaction_http_response_body xmlResult -<< inTxn
  where
    pickleResult = arr $ pickleDoc xpResult

