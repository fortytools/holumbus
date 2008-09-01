-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Search
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  The search web-service for the Hayoo Haskell API search engine.
 
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows -fno-warn-type-defaults #-}

module Hayoo.Search where

import Prelude

import Data.Function
import Data.Maybe

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntSet as IS

import qualified Data.ByteString.UTF8 as B

import Network.HTTP (urlDecode)

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted.OneFile
       ( Persistent
       )
import Holumbus.Index.SmallDocuments
       ( SmallDocuments
       )
import Holumbus.Index.Cache
import Holumbus.Index.Common

import Holumbus.Query.Language.Grammar
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy

import Network.Server.Janus.Core
       ( JanusStateArrow
       , Shader
       , ShaderCreator
       , mkDynamicCreator
       )
import Network.Server.Janus.XmlHelper
       ( getValDef
       , setVal
       )
import Network.Server.Janus.JanusPaths

import System.Time

import System.Log.Logger
import System.Log.Handler.Simple

import Control.Concurrent  -- For the global MVar
import Control.Monad

import Hayoo.Common
import Hayoo.Parser
import Hayoo.HTML

data Core = Core
  { index :: Persistent
  , documents :: SmallDocuments FunctionInfo
  , cache :: Cache
  }

-- | Weights for context weighted ranking.
contextWeights :: [(Context, Score)]
contextWeights = [ ("name", 0.9)
                 , ("partial", 0.8)
                 , ("module", 0.7)
                 , ("hierarchy", 0.6)
                 , ("package", 0.5)
                 , ("signature", 0.4)
                 , ("description", 0.2)
                 , ("normalized", 0.1)
                 ]

-- | The place where the filename of the index file is stored in the server configuration XML file.
_shader_config_index :: JanusPath
_shader_config_index = jp "/shader/config/@index"

-- | The place where the filename of the documents file is stored in the server configuration XML file.
_shader_config_documents :: JanusPath
_shader_config_documents = jp "/shader/config/@documents"

-- | The place where the filename of the cache file is stored in the server configuration XML file.
_shader_config_cache :: JanusPath
_shader_config_cache = jp "/shader/config/@cache"

-- | The path of the logfile to use.
_shader_config_log :: JanusPath
_shader_config_log = jp "/shader/config/@log"

-- | Just an alias with explicit type.
loadIndex :: FilePath -> IO Persistent
loadIndex = loadFromFile

-- | Just an alias with explicit type.
loadDocuments :: FilePath -> IO (SmallDocuments FunctionInfo)
loadDocuments = loadFromFile

hayooShader :: ShaderCreator
hayooShader = mkDynamicCreator $ proc (conf, _) -> do
  -- Load the files and create the indexes.
  inv <- (arrIO $ loadIndex) <<< (getValDef _shader_config_index "hayoo-index.bin") -< conf
  doc <- (arrIO $ loadDocuments) <<< (getValDef _shader_config_documents "hayoo-docs.bin") -< conf
  cac <- (arrIO $ createCache) <<< (getValDef _shader_config_cache "hayoo-cache.db") -< conf
  hdl <- (arrIO $ (flip fileHandler) INFO) <<< (getValDef _shader_config_log "hayoo.log") -< conf
  arrIO $ (\h -> updateGlobalLogger rootLoggerName (setHandlers [h])) -< hdl
  arrIO $ (\_ -> updateGlobalLogger rootLoggerName (setLevel INFO)) -< ()
  -- Store the data in MVar's to allow shared access.
  midc <- arrIO $ newMVar -< Core inv doc cac
  returnA -< hayooService midc

hayooService :: MVar Core -> Shader
hayooService midc = proc inTxn -> do
  -- Because index access is read only, the MVar's are just read to make them avaliable again.
  idc      <- arrIO $ readMVar                                                     -< midc
  -- Extract the query from the incoming transaction and log it to stdout.
  request  <- getValDef (_transaction_http_request_cgi_ "@query") ""               -< inTxn
  start    <- readDef 0 <<< getValDef (_transaction_http_request_cgi_ "@start") "" -< inTxn
  -- Output some information about the request.
  arrLogRequest                                                                    -< inTxn
  state    <- arr $ PickleState start                                              -<< (cache idc)
  -- Parse the query and generate a result or an error depending on the parse result.
  response <- writeString state <<< (genError ||| genResult) <<< arrParseQuery     -<< (request, idc)
  -- Put the response value into the transaction.
  setVal _transaction_http_response_body response                                  -<< inTxn    
    where
    -- Transforms the result and the status information to HTML by pickling it using the XML picklers.
    writeString s = pickleStatusResult s >>> (writeDocumentToString [(a_no_xml_pi, v_1), (a_output_encoding, utf8)])
    pickleStatusResult s = arrFilterStatusResult >>> xpickleVal (xpStatusResult s)
    readDef d = arr $ fromMaybe d . readM

-- | Enable handling of parse errors from 'read'.
readM :: (Read a, Monad m) => String -> m a
readM s = case reads s of
            [(x, "")] -> return x
            _         -> fail "No parse"

-- | Perform some postprocessing on the status and the result.
arrFilterStatusResult :: ArrowXml a => a StatusResult StatusResult
arrFilterStatusResult = arr $ (\(s, r) -> (s, filterResult r))
  where
  filterResult (Result dh wh) = Result dh (M.filterWithKey (\w _ -> not ("->" `L.isInfixOf` w)) wh)

-- | Tries to parse the search string and returns either the parsed query or an error message.
arrParseQuery :: ArrowXml a => a (String, Core) (Either (String, Core) (Query, Core))
arrParseQuery =  (first arrDecode)
                 >>>
                 (arr $ (\(r, idc) -> either (\m -> Left (m, idc)) (\q -> Right (q, idc)) (parseQuery r)))

-- | Decode any URI encoded entities and transform to unicode.
arrDecode :: Arrow a => a String String
arrDecode = arr $ fst . utf8ToUnicode . urlDecode

-- | Log a request to stdout.
arrLogRequest :: JanusStateArrow XmlTree ()
arrLogRequest = proc inTxn -> do
  -- Extract remote host and the search string from the incoming transaction.
  remHost <- getValDef (_transaction_tcp_remoteHost) ""                -< inTxn
  rawRequest <- getValDef (_transaction_http_request_cgi_ "@query") "" -< inTxn
  start <- getValDef (_transaction_http_request_cgi_ "@start") "0"     -< inTxn
  proxy <- getValDef (_transaction_http_request_header_ "X-Forwarded-For") "No proxy" -< inTxn
  userAgent <- getValDef (_transaction_http_request_header_ "User-Agent") "No user agent" -< inTxn
  -- Decode URI encoded entities in the search string.
  decodedRequest <- arrDecode                                          -< rawRequest
  -- Get the current date and time.
  unixTime <- arrIO $ (\_ -> getClockTime)                             -< ()
  currTime <- arr $ calendarTimeToString . toUTCTime                   -< unixTime
  -- Output all the collected information from above to stdout.
  arrIO $ infoM "Hayoo.Request" -< (currTime ++ "\t" ++ 
                                    remHost ++ "\t "++ 
                                    proxy ++ "\t" ++ 
                                    userAgent ++ "\t" ++
                                    rawRequest ++ "\t" ++ 
                                    decodedRequest ++ "\t" ++ 
                                    start
                                   )

-- | Customized Hayoo! ranking function. Preferres exact matches and matches in Prelude.
hayooRanking :: [(Context, Score)] -> [String] -> DocId -> DocInfo FunctionInfo -> DocContextHits -> Score
hayooRanking ws ts _ di dch = baseScore * (if isInPrelude then 3.0 else 1.0) * (if isExactMatch then 3.0 else 1.0)
  where
  baseScore = M.foldWithKey calcWeightedScore 0.0 dch
  isExactMatch = L.foldl' (\r t -> t == (title $ document di) || r) False ts
  isInPrelude = maybe False (\fi -> (B.toString $ moduleName fi) == "Prelude") (custom $ document di)
  calcWeightedScore :: Context -> DocWordHits -> Score -> Score
  calcWeightedScore c h r = maybe r (\w -> r + ((w / mw) * count)) (lookupWeight ws)
    where
    count = fromIntegral $ M.fold ((+) . IS.size) 0 h
    mw = snd $ L.maximumBy (compare `on` snd) ws
    lookupWeight [] = Nothing
    lookupWeight (x:xs) = if fst x == c then
                            if snd x /= 0.0
                            then Just (snd x)
                            else Nothing
                          else lookupWeight xs

-- | This is the core arrow where the request is finally processed.
genResult :: ArrowXml a => a (Query, Core) (String, Result FunctionInfo)
genResult = --ifP (\(q, _) -> checkWith ((> 1) . length) q)
              (proc (q, idc) -> do
                res <- (arr $ makeQuery)           -< (q, idc) -- Execute the query
                cfg <- (arr $ (\q' -> RankConfig (hayooRanking contextWeights (extractTerms q')) wordRankByCount)) -< q
                rnk <- (arr $ rank cfg)            -<< res -- Rank the results
                (arr $ (\r -> (msgSuccess r , r))) -< rnk -- Include a success message in the status
              )

              -- Tell the user to enter more characters if the search terms are too short.
--              (arr $ (\(_, _) -> ("Please enter some more characters.", emptyResult)))
-- TH 12.04.2008: Preventing one-character queries makes searches for operators almost impossible.

-- | Generate a success status response from a query result.
msgSuccess :: Result FunctionInfo -> String
msgSuccess r = if sd == 0 then "Nothing found yet." 
               else "Found " ++ (show sd) ++ " " ++ ds ++ " and " ++ (show sw) ++ " " ++ cs ++ "."
                 where
                 sd = sizeDocHits r
                 sw = sizeWordHits r
                 ds = if sd == 1 then "result" else "results"
                 cs = if sw == 1 then "completion" else "completions"

-- | This is where the magic happens! This helper function really calls the 
-- processing function which executes the query.
makeQuery :: (Query, Core) -> Result FunctionInfo
makeQuery (q, Core i d _) = processQuery cfg i d q
                           where
                           cfg = ProcessConfig (FuzzyConfig False True 1.0 []) True 50

-- | Generate an error message in case the query could not be parsed.
genError :: ArrowXml a => a (String, Core) (String, Result FunctionInfo)
genError = arr $ (\(msg, _) -> (tail $ dropWhile ((/=) ':') msg, emptyResult))
