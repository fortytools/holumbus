-- ----------------------------------------------------------------------------

{- |
  Module     : HayooSearch
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
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Network.HTTP (urlDecode)

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted.OneFile
       ( Persistent
       )
import Holumbus.Index.Documents
       ( Documents
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
import System.IO.Unsafe

import System.Log.Logger
import System.Log.Handler.Simple

import Control.Concurrent  -- For the global MVar

import Hayoo.Common
import Hayoo.Parser

data Core = Core
  { index :: Persistent
  , documents :: Documents FunctionInfo
  , cache :: Cache
  }

data PickleState = PickleState
  { psStart :: Int
  , psCache :: Cache
  }

-- Status information of query processing.
type StatusResult = (String, Result FunctionInfo)

-- | Weights for context weighted ranking.
contextWeights :: [(Context, Score)]
contextWeights = [ ("name", 0.9)
                 , ("partial", 0.8)
                 , ("module", 0.7)
                 , ("hierarchy", 0.6)
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
loadDocuments :: FilePath -> IO (Documents FunctionInfo)
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
  isInPrelude = maybe False (\fi -> moduleName fi == "Prelude") (custom $ document di)
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

-- | The combined pickler for the status response and the result.
xpStatusResult :: PickleState -> PU StatusResult
xpStatusResult s = xpDivId "result" $ xpPair xpStatus (xpResultHtml s)

-- | Enclose the status message in a <div> tag.
xpStatus :: PU String
xpStatus = xpDivId "status" xpText

-- | The HTML Result pickler. Extracts the maximum word score for proper scaling in the cloud.
xpResultHtml :: PickleState -> PU (Result FunctionInfo)
xpResultHtml s = xpWrap (\((_, wh), (_, dh)) -> Result dh wh, \r -> ((maxScoreWordHits r, wordHits r), (sizeDocHits r, docHits r))) 
                 (xpPair xpWordHitsHtml (xpDocHitsHtml s))

-- | Wrapping something in a <div> element with id attribute.
xpDivId :: String -> PU a -> PU a
xpDivId i p = xpElem "div" (xpAddFixedAttr "id" i p)

-- | Wrapping something in a <div> element with class attribute.
xpDivClass :: String -> PU a -> PU a
xpDivClass c p = xpElem "div" (xpAddFixedAttr "class" c p)

-- | Set the class of the surrounding element.
xpClass :: String -> PU a -> PU a
xpClass c p = xpAddFixedAttr "class" c p

-- | Seth the id of the surrounding element.
xpId :: String -> PU a -> PU a
xpId i p = xpAddFixedAttr "id" i p

-- | Append some text after pickling something else.
xpAppend :: String -> PU a -> PU a
xpAppend t p = xpWrap (\(v, _) -> v, \v -> (v, t)) (xpPair p xpText)

-- | Prepend some text before pickling something else.
xpPrepend :: String -> PU a -> PU a
xpPrepend t p = xpWrap (\(_, v) -> v, \v -> (t, v)) (xpPair xpText p)

-- | The HTML pickler for the document hits. Will be sorted by score. Also generates the navigation.
xpDocHitsHtml :: PickleState -> PU (Int, DocHits FunctionInfo)
xpDocHitsHtml s = xpWrap (\(d, n) -> (n, d) ,\(n, d) -> (d, n)) (xpPair (xpDocs (psCache s)) (xpPager (psStart s)))
  where
  xpDocs c = xpDivId "documents" $ xpElem "table" $ xpId "functions" (xpWrap (IM.fromList, toListSorted) (xpList $ xpDocInfoHtml c))
  toListSorted = take pageLimit . drop (psStart s) . reverse . L.sortBy (compare `on` (docScore . fst . snd)) . IM.toList -- Sort by score

xpPager :: Int -> PU Int
xpPager s = xpWrap wrapper (xpOption $ xpDivId "pager" (xpWrap (\_ -> 0, makePager s pageLimit) (xpOption xpickle)))
  where
  wrapper = (undefined, \v -> if v > 0 then Just v else Nothing)

xpDocInfoHtml :: HolCache c => c -> PU (DocId, (DocInfo FunctionInfo, DocContextHits))
xpDocInfoHtml c = xpWrap (undefined, docToHtml) (xpPair xpQualified xpAdditional)
  where
  docToHtml (_, (DocInfo (Document _ _ Nothing) _, _)) = error "Expecting custom information for document"
  docToHtml (i, (DocInfo (Document t u (Just (FunctionInfo m s p l))) _, _)) = (((modLink u, m), (u, t), s), ((pkgLink p, p), (getDesc i,l)))
    where
    modLink = takeWhile ((/=) '#')
    pkgLink "gtk2hs" = "http://www.haskell.org/gtk2hs"
    pkgLink "base" = "http://hackage.haskell.org/packages/archive/base/latest/doc/html/"
    pkgLink p' = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ p'
    getDesc = unsafePerformIO . getDocText c "description" 
  xpQualified = xpElem "tr" $ xpClass "function" $ xpTriple xpModule xpFunction xpSignature
    where
    xpModule = xpCell "module" $ xpPair (xpElem "a" $ xpClass "module" $ xpAttr "href" $ xpText) (xpAppend "." $ xpText)
    xpFunction = xpCell "function" $ xpPair (xpElem "a" $ xpClass "function" $ xpAttr "href" $ xpText) xpText
    xpSignature = xpCell "signature" $ xpPrepend ":: " xpSigDecl
  xpAdditional = xpElem "tr" $ xpClass "description" $ xpPair xpPackage xpDescSource
    where
    xpPackage = xpCell "package" $ xpPair (xpElem "a" $ xpClass "package" $ xpAttr "href" $ xpText) xpText
    xpDescSource = xpCell "description" $ xpAddFixedAttr "colspan" "2" $ xpPair xpDescription xpSource
    xpDescription = xpWrap (undefined, limitDescription) (xpElem "span" $ xpClass "description" $ xpText)
    xpSource = xpOption $ (xpElem "span" $ xpClass "source" $ xpElem "a" $ xpClass "source" $ xpAppend "Source" $ xpAttr "href" $ xpText)
    limitDescription = maybe "No description. " (\d -> if length d > 100 then (take 100 d) ++ "... " else d ++ " ")

data Signature = Signature String
               | Declaration String

xpSigDecl :: PU String
xpSigDecl = xpWrap (undefined, makeSignature) (xpAlt tag [xpSig, xpDecl])
  where
  xpSig = xpWrap (Signature, \(Signature s) -> s) xpText
  xpDecl = xpWrap (Declaration, \(Declaration s) -> s) (xpElem "span" $ xpClass "declaration" $ xpText)
  tag (Signature _) = 0
  tag (Declaration _) = 1
  makeSignature s = if (s == "data") || (s == "type") || (s == "newtype") || (s == "class")
                    then Declaration s
                    else Signature s

xpFixedElem :: String -> PU a -> PU a
xpFixedElem e p = xpWrap (\(_, v) -> v, \v -> (" ", v)) (xpPair (xpElem e xpText) p)

xpCell :: String -> PU a -> PU a
xpCell c p = xpElem "td" $ xpClass c $ p

xpWordHitsHtml :: PU (Score, WordHits)
xpWordHitsHtml = xpDivId "words" $ xpElem "p" $ xpClass "cloud" $ xpWrap (fromListSorted, toListSorted) (xpList xpWordHitHtml)
  where
  fromListSorted _ = (0.0, M.empty)
  toListSorted (s, wh) = map (\a -> (s, a)) $ L.sortBy (compare `on` fst) $ M.toList wh -- Sort by word
  xpWordHitHtml = xpWrap (wordFromHtml, wordToHtml) (xpWordHtml)
    where
    wordToHtml (m, (w, (WordInfo ts s, _))) = ((head ts, w), ((s, m), w))
    wordFromHtml ((t, _), ((s, m), w)) = (m, (w, (WordInfo [t] s, M.empty)))
    xpWordHtml = xpAppend " " $ xpElem "a" $ xpClass "cloud" $ xpPair xpCloudLink xpScore

xpCloudLink :: PU (String, Word)
xpCloudLink = xpAttr "href" $ xpPair (xpPrepend "javascript:replaceInQuery(\"" $ xpAppend "\",\"" xpEscape) (xpAppend "\")" $ xpEscape)

xpEscape :: PU String
xpEscape = xpWrap (unescape, escape) xpText
  where
  unescape = filter ((/=) '\\')
  escape [] = []
  escape (x:xs) = if x == '\'' then "\\'" ++ escape xs else x : (escape xs)

xpScore :: PU ((Score, Score), Word)
xpScore = xpElem "span" $ xpPair (xpAttr "class" $ xpWrap (scoreFromHtml, scoreToHtml) xpText) xpText
  where
  scoreToHtml (v, top) = "cloud" ++ (show $ round (weightScore 1 9 top v))
  scoreFromHtml _ = (0.0, 0.0)
  weightScore mi ma to v = ma - ((to - v) / to) * (ma - mi)

pageLimit :: Int
pageLimit = 10

data Pager = Pager 
  { prevPage  :: Maybe Int -- == last predPages
  , predPages :: [(Int, Int)]
  , currPage  :: Int
  , succPages :: [(Int, Int)]
  , nextPage  :: Maybe Int -- == head succPages
  }

instance XmlPickler Pager where
  xpickle = xpWrap convert (xp5Tuple xpPrevPage xpPages xpCurrPage xpPages xpNextPage)
    where
    convert = (\(pv, pd, c, sc, nt) -> Pager pv pd c sc nt, \(Pager pv pd c sc nt) -> (pv, pd, c, sc, nt))
    xpPrevPage = xpOption $ xpElem "a" $ xpClass "previous" $ xpAppend "<" $ xpAttr "href" xpShowPage
    xpCurrPage = xpElem "span" $ xpClass "current" $ xpPrim
    xpNextPage = xpOption $ xpElem "a" $ xpClass "next" $ xpAppend ">" $ xpAttr "href" xpShowPage
    xpPages = xpList $ xpElem "a" $ xpClass "page" $ xpPair (xpAttr "href" $ xpPrepend "javascript:showPage(" $ xpAppend ")" $ xpPrim) xpPrim
    xpShowPage = xpPrepend "javascript:showPage(" $ xpAppend ")" $ xpPrim

-- Start element (counting from zero), elements per page, total number of elements.
makePager :: Int -> Int -> Int -> Maybe Pager
makePager s p n = if n > p then Just $ Pager pv (drop (length pd - 10) pd) (length pd + 1) (take 10 sc) nt else Nothing
  where
  pv = if s < p then Nothing else Just (s - p)
  nt = if s + p >= n then Nothing else Just (s + p)
  pd = map (\x -> (x, x `div` p + 1)) $ genPred s []
    where
    genPred rp tp = let np = rp - p in if np < 0 then tp else genPred np (np:tp)
  sc = map (\x -> (x, x `div` p + 1)) $ genSucc s []
    where
    genSucc rs ts = let ns = rs + p in if ns >= n then ts else genSucc ns (ts ++ [ns])
