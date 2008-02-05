-- ----------------------------------------------------------------------------

{- |
  Module     : WebSearch
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  An example of how Holumbus can be used together with the Janus application
  server to create a web service.
 
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows -fno-warn-type-defaults #-}

module Network.Server.Janus.Shader.WebSearch where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Network.HTTP (urlDecode)

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Index.Documents (Documents)
import Holumbus.Index.Common

import Holumbus.Query.Language
import Holumbus.Query.Parser
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy

import Network.Server.Janus.Core (Shader, ShaderCreator)
import qualified Network.Server.Janus.Core as J
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

import System.Time

import Control.Concurrent  -- For the global MVar

-- Status information of query processing.
type StatusResult = (String, Result)

_shader_config_index :: JanusPath
_shader_config_index = jp "/shader/config/@index"

_shader_config_documents :: JanusPath
_shader_config_documents = jp "/shader/config/@documents"

loadIndex :: FilePath -> IO InvIndex
loadIndex = loadFromFile

loadDocuments :: FilePath -> IO Documents
loadDocuments = loadFromFile

websearchShader :: ShaderCreator
websearchShader = J.mkDynamicCreator $ proc (conf, _) -> do
  idx <- (arrIO $ loadIndex) <<< (getValDef _shader_config_index "") -< conf
  doc <- (arrIO $ loadDocuments) <<< (getValDef _shader_config_documents "") -< conf
  mix <- arrIO $ newMVar -< idx
  moc <- arrIO $ newMVar -< doc
  returnA -< websearchService mix moc

websearchService :: (HolIndex i, HolDocuments d) => MVar i -> MVar d -> Shader
websearchService mix moc = proc inTxn -> do
  idx      <- arrIO $ readMVar                                             -< mix
  doc      <- arrIO $ readMVar                                             -< moc
  request  <- getValDef (_transaction_http_request_cgi_ "@query") ""       -< inTxn
  arrLogRequest                                                            -< inTxn
  response <- writeString <<< (genError ||| genResult) <<< (arrParseQuery) -< (request, (idx, doc))
  setVal _transaction_http_response_body response                          -<< inTxn    
    where
    writeString = pickleStatusResult >>> (writeDocumentToString [(a_no_xml_pi, v_1), (a_output_encoding, utf8)])
    pickleStatusResult = xpickleVal xpStatusResult

arrParseQuery :: (HolIndex i, HolDocuments d, ArrowXml a) => a (String, (i, d)) (Either (String, (i, d)) (Query, (i, d)))
arrParseQuery = (first arrDecode)
                >>>
                (arr $ (\(r, ind) -> either (\m -> Left (m, ind)) (\q -> Right (q, ind)) (parseQuery r)))

arrDecode :: Arrow a => a String String
arrDecode = arr $ fst . utf8ToUnicode . urlDecode

arrLogRequest :: JanusArrow J.Context XmlTree ()
arrLogRequest = proc inTxn -> do
  remHost <- getValDef (_transaction_tcp_remoteHost) ""                -< inTxn
  rawRequest <- getValDef (_transaction_http_request_cgi_ "@query") "" -< inTxn
  decodedRequest <- arrDecode                                          -< rawRequest
  unixTime <- arrIO $ (\_ -> getClockTime)                             -< ()
  currTime <- arr $ calendarTimeToString . toUTCTime                   -< unixTime
  arrIO $ putStrLn -< (currTime ++ " - " ++ remHost ++ " - " ++ rawRequest ++ " - " ++ decodedRequest)

genResult :: (HolIndex i, HolDocuments d, ArrowXml a) => a (Query, (i, d)) (String, Result)
genResult = let 
              rankCfg = RankConfig (docRankWeightedByCount weights) (wordRankWeightedByCount weights)
              weights = [("title", 0.8), ("keywords", 0.6), ("headlines", 0.4), ("content", 0.2)]
            in
            ifP (\(q, _) -> checkWith ((> 1) . length) q)
              ((arr $ (\(q, ind) -> (makeQuery ind q, ind)))
              >>>
              (first $ arr $ rank rankCfg)
              >>>
              (arr $ (\(r, _) -> (msgSuccess r , r))))
              
              (arr $ (\(_, _) -> ("Please enter some more characters.", emptyResult)))

msgSuccess :: Result -> String
msgSuccess r = if sd == 0 then "Nothing found yet." 
               else "Found " ++ (show sd) ++ " " ++ ds ++ " and " ++ (show sw) ++ " " ++ cs ++ "."
                 where
                 sd = sizeDocHits r
                 sw = sizeWordHits r
                 ds = if sd == 1 then "document" else "documents"
                 cs = if sw == 1 then "completion" else "completions"

-- | This is where the magic happens!
makeQuery :: (HolIndex i, HolDocuments d) => (i, d) -> Query -> Result
makeQuery (i, d) q = processQuery cfg i d (optimize q)
                       where
                       cfg = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True

genError :: (HolIndex i, HolDocuments d, ArrowXml a) => a (String, (i, d)) (String, Result)
genError = arr $ (\(msg, _) -> (msg, emptyResult))

xpStatusResult :: PU StatusResult
xpStatusResult = xpElem "div" $ xpAddFixedAttr "id" "result" $ xpPair xpStatus xpResultHtml

xpStatus :: PU String
xpStatus = xpElem "div" $ xpAddFixedAttr "id" "status" xpText

-- The HTML Result pickler

xpResultHtml :: PU Result
xpResultHtml = xpWrap (\((_, wh), dh) -> Result dh wh, \r -> ((maxScoreWordHits r, wordHits r), docHits r)) 
               (xpPair xpWordHitsHtml xpDocHitsHtml)

-- | Wrapping something in a <div> element with id attribute.
xpDivId :: String -> PU a -> PU a
xpDivId i p = xpElem "div" (xpAddFixedAttr "id" i p)

-- | Set the class of the surrounding element.
xpClass :: String -> PU a -> PU a
xpClass c p = xpAddFixedAttr "class" c p

xpAppend :: String -> PU a -> PU a
xpAppend t p = xpWrap (\(v, _) -> v, \v -> (v, t)) (xpPair p xpText)

xpPrepend :: String -> PU a -> PU a
xpPrepend t p = xpWrap (\(_, v) -> v, \v -> (t, v)) (xpPair xpText p)

-- | The HTML pickler for the document hits. Will be sorted by score.
xpDocHitsHtml :: PU DocHits
xpDocHitsHtml = xpDivId "documents" (xpWrap (IM.fromList, toListSorted) (xpList xpDocHitHtml))
  where
  toListSorted = reverse . L.sortBy (compare `on` (docScore . fst . snd)) . IM.toList -- Sort by score
  xpDocHitHtml = xpElem "p" $ xpClass "document" $ xpDocInfoHtml

xpDocInfoHtml :: PU (DocId, (DocInfo, DocContextHits))
xpDocInfoHtml = xpWrap (docFromHtml, docToHtml) (xpTriple xpTitleHtml xpContextsHtml xpURIHtml)

docToHtml :: (DocId, (DocInfo, DocContextHits)) -> ((URI, Title), DocContextHits, URI)
docToHtml (_, (DocInfo (title, uri) _, dch)) = ((uri, title), dch, uri)

docFromHtml :: (Document, DocContextHits, URI) -> (DocId, (DocInfo, DocContextHits))
docFromHtml ((uri, title), dch, _) = (0, (DocInfo (title, uri) 0.0, dch))

xpTitleHtml :: PU (URI, Title)
xpTitleHtml = xpElem "div" $ xpClass "title" $ xpElem "a" $ xpClass "link" $ (xpPair (xpAttr "href" xpText) xpText)

xpContextsHtml :: PU DocContextHits
xpContextsHtml = xpElem "div" $ xpClass "contexts" $ xpWrap (M.fromList, M.toList) (xpList xpContextHtml)

xpContextHtml :: PU (Context, DocWordHits)
xpContextHtml = xpPair (xpElem "span" $ xpClass "context" $ xpAppend ": " $ xpText) xpWordsHtml

xpWordsHtml :: PU DocWordHits
xpWordsHtml = xpWrap (M.fromList, M.toList) (xpList (xpPair (xpAppend " " $ xpText) xpZero))

xpURIHtml :: PU String
xpURIHtml = xpElem "div" $ xpClass "uri" $ xpText

xpWordHitsHtml :: PU (Score, WordHits)
xpWordHitsHtml = xpDivId "words" $ xpElem "p" $ xpClass "cloud" $ xpWrap (fromListSorted, toListSorted) (xpList xpWordHitHtml)
  where
  fromListSorted _ = (0.0, M.empty)
  toListSorted (s, wh) = map (\a -> (s, a)) $ L.sortBy (compare `on` fst) $ M.toList wh -- Sort by word
  xpWordHitHtml = xpWrap (wordFromHtml, wordToHtml) (xpWordHtml)

wordToHtml :: (Score, (Word, (WordInfo, WordContextHits))) -> ((String, Word), ((Score, Score), Word))
wordToHtml (m, (w, (WordInfo ts s, _))) = ((head ts, w), ((s, m), w))

wordFromHtml :: ((String, Word), ((Score, Score), Word)) -> (Score, (Word, (WordInfo, WordContextHits)))
wordFromHtml ((t, _), ((s, m), w)) = (m, (w, (WordInfo [t] s, M.empty)))

xpWordHtml :: PU ((String, Word), ((Score, Score), Word))
xpWordHtml = xpAppend " " $ xpElem "a" $ xpClass "cloud" $ xpPair xpLink xpScore

xpLink :: PU (String, Word)
xpLink = xpAttr "href" $ xpPair (xpPrepend "javascript:replaceInQuery('" $ xpAppend "','" xpText) (xpAppend "')" $ xpText)

xpScore :: PU ((Score, Score), Word)
xpScore = xpElem "span" $ xpPair (xpAttr "class" $ xpWrap (scoreFromHtml, scoreToHtml) xpText) xpText

weightScore :: Score -> Score -> Score -> Score -> Score
weightScore mi ma to v = ma - ((to - v) / to) * (ma - mi)

scoreToHtml :: (Score, Score) -> String
scoreToHtml (v, top) = "cloud" ++ (show $ round (weightScore 1 9 top v))

scoreFromHtml :: String -> (Score, Score)
scoreFromHtml _ = (0.0, 0.0)

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y
