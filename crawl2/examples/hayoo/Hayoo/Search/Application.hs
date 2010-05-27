-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.SearchApplication
  Copyright  : Copyright (C) 2010 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The search web-service for the Hayoo Haskell API search engine.

-}

-- ----------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

module Hayoo.Search.Application (hayooApplication, Core (Core) ) where

import Data.Function
import Data.Maybe
import Data.Char

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import qualified Data.ByteString.UTF8 as B

import Data.ByteString.Lazy.Char8 (pack)

import Network.URI (unEscapeString)

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Unicode

import Hayoo.IndexTypes
{-
import Holumbus.Index.Inverted.CompressedPrefixMem
       ( InvertedOSerialized
       )
import Holumbus.Index.SmallDocuments
       ( SmallDocuments
       )
-}
import Holumbus.Index.Cache
import Holumbus.Index.Common

import Holumbus.Query.Language.Grammar
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy

import Holumbus.Utility

import Hack
import Hack.Contrib.Request (params, path, host)

import System.Time

import System.Log.Logger

import Control.Concurrent  -- For the global MVar

import Hayoo.Common
import Hayoo.Parser
import Hayoo.HTML
import Hayoo.JSON

type Template = XmlTree

data Core = Core
  { index 	:: CompactInverted
  , documents 	:: SmallDocuments FunctionInfo
  , cache 	:: Cache
  , template 	:: Template
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

-- | Mutliplier for score when result is from "base" package.
factBase :: Score
factBase = 1.5

-- | Mutliplier for score when result is from "Prelude" module.
factPrelude :: Score
factPrelude = 2.0

-- | Multiplier for score when result is an exact match.
factExact :: Score
factExact = 3.0

-- | Generate the actual response
hayooApplication :: MVar Core -> Env -> IO Response
hayooApplication midct env = let p = params env in do
  request <- return $ getValDef p "query" ""
  start   <- return $ readDef 0 (getValDef p "start" "")
  static  <- return $ readDef True (getValDef p "static" "")
  json    <- return $ isJson (path env)
  -- Output some information about the request.
  logRequest env
  -- Because index access is read only, the MVar's are just read to make them avaliable again.
  idct    <- readMVar midct
  -- Put all information relevant for rendering into a container
  state   <- return $ (request, start, static, (cache idct), (template idct))
  -- If the query is empty, just render an empty page
  resp    <- return $ if L.null request then renderEmpty json idct else renderResult state json (request, idct)
  -- Determine the mime type of the response
  mime    <- return $ if json then "application/json" else "text/html"
  -- Return the actual response
  return $ Response { status = 200, headers = [ ("Content-Type", mime) ], body = pack resp }
  where
    -- Just render an empty page/JSON answer
    renderEmpty j idct = if j then writeJson else writeHtml
      where
      writeJson = renderEmptyJson
      writeHtml = head $ runLA (writeDocumentToString htmlOptions) (template idct)

    -- Parse the query and generate a result or an error depending on the parse result.
    renderResult (r, s, i, c, t) j inp = head $ runLA processArrow inp
      where
      processArrow = arrParseQuery >>> (genError ||| genResult) >>> if j then writeJson c else writeHtml (PickleState r s i c t)
      writeJson ca = arr $ (\sr -> renderJson sr ca)
      writeHtml ps = pickleStatusResult ps >>> applyTemplate ps >>>
        (writeDocumentToString $ (a_no_xml_pi, if psStatic ps then v_0 else v_1):htmlOptions)

    -- Apply the template if necessary
    applyTemplate ps =
      if psStatic ps then
        insertTreeTemplate (constA $ psTemplate ps) [ hasAttrValue "id" (== "result") :-> this ] >>>
        staticSubstitutions ps >>>
        addXHtmlDoctypeStrict
      else arr id
    -- Do the real pickle work
    pickleStatusResult ps =
      arrFilterStatusResult >>> xpickleVal (xpStatusResult ps)

    -- Check requested path for JSON
    isJson f = extension f == "json"
    -- Default HTML render options
    htmlOptions = [(a_output_encoding, utf8), (a_indent,v_1), (a_output_html, v_1)]

-- Read or use default value
readDef :: Read a => a -> String -> a
readDef d = fromMaybe d . readM

    
-- | Determine the file name extension of a path
extension :: String -> String
extension fn = go (reverse fn) ""
  where
  go []      _   = ""
  go ('.':_) ext = ext
  go (x:s)   ext = go s (x:ext)

getValDef :: [(String,String)] -> String -> String -> String
getValDef l k d = fromMaybe d (lookup k l)

staticSubstitutions :: ArrowXml a => PickleState -> a XmlTree XmlTree
staticSubstitutions ps = processTopDown setQuery
  where
  setQuery = processAttrl (changeAttrValue (\_ -> urlDecode $ psQuery ps) `when` hasName "value") `when` (isElem >>> hasAttrValue "id" (== "querytext"))

-- | Enable handling of parse errors from 'read'.
readM :: (Read a, Monad m) => String -> m a
readM s = case reads s of
            [(x, "")] -> return x
            _         -> fail "No parse"

-- | Proper URL decoding including substitution of "the annoying +" (tm)
urlDecode :: String -> String
urlDecode = unEscapeString . replaceElem '+' ' '

replaceElem :: Eq a => a -> a -> [a] -> [a]
replaceElem x y = map (\z -> if z == x then y else z)

-- | Perform some postprocessing on the status and the result.
arrFilterStatusResult :: ArrowXml a => a StatusResult StatusResult
arrFilterStatusResult = arr $ (\(s, r, m, p) -> (s, filterResult r, m, p))
  where
  filterResult (Result dh wh) = Result dh (M.filterWithKey (\w _ -> not ("->" `L.isInfixOf` w)) wh)

-- | Tries to parse the search string and returns either the parsed query or an error message.
arrParseQuery :: ArrowXml a => a (String, Core) (Either (String, Core) (Query, Core))
arrParseQuery =  (first (arr decode))
                 >>>
                 (arr $ (\(r, idc) -> either (\m -> Left (m, idc)) (\q -> Right (q, idc)) (parseQuery r)))

-- | Decode any URI encoded entities and transform to unicode.
decode :: String -> String
decode = fst . utf8ToUnicode . urlDecode

-- | Log a request to stdout.
logRequest :: Env -> IO ()
logRequest env = do
  -- Extract remote host and the search string from the incoming transaction.
  remHost    <- return $ host env
  rawRequest <- return $ getValDef (params env) "query" ""
  start      <- return $ getValDef (params env) "start" "0"
  userAgent  <- return $ getValDef (http env) "User-Agent" "No user agent"
  -- Decode URI encoded entities in the search string.
  decodedRequest <- return $ decode rawRequest
  -- Get the current date and time.
  unixTime <- getClockTime
  currTime <- return $ calendarTimeToString $ toUTCTime unixTime
  -- Output all the collected information from above to stdout.
  infoM "Hayoo.Request" (currTime ++ "\t" ++
                         remHost ++ "\t "++
                         userAgent ++ "\t" ++
                         rawRequest ++ "\t" ++
                         decodedRequest ++ "\t" ++
                         start
                        )

-- | Customized Hayoo! ranking function. Preferres exact matches and matches in Prelude and base.
hayooRanking :: [(Context, Score)] -> [String] -> DocId -> DocInfo FunctionInfo -> DocContextHits -> Score
hayooRanking ws ts _ di dch = baseScore
                            * factModule
                            * (if isInPrelude then factPrelude else 1.0)
                            * (if isExactMatch then factExact else 1.0)
                            * (if isInBase then factBase else 1.0)
  where
  baseScore = M.foldWithKey calcWeightedScore 0.0 dch
  isExactMatch = L.foldl' (\r t -> t == (title $ document di) || r) False ts
  isInPrelude = maybe False (\fi -> (B.toString $ moduleName fi) == "Prelude") (custom $ document di)
  isInBase = maybe False (\fi -> (B.toString $ package fi) == "base") (custom $ document di)
  factModule = maybe 1.0 (\fi -> 1.0 / (fromIntegral $ length $ split "." $ B.toString $ moduleName fi)) (custom $ document di)
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
genResult :: ArrowXml a => a (Query, Core) StatusResult
genResult = ifP (\(q, _) -> checkWith isEnough q)
              (proc (q, idc) -> do
                res <- (arr $ makeQuery)           -< (q, idc) -- Execute the query
                cfg <- (arr $ (\q' -> RankConfig (hayooRanking contextWeights (extractTerms q')) wordRankByCount)) -< q
                rnk <- (arr $ rank cfg)            -<< res -- Rank the results
                (arr $ (\r -> (msgSuccess r, r, genModules r, genPackages r))) -< rnk -- Include a success message in the status
              )
              -- Tell the user to enter more characters if the search terms are too short.
              (arr $ (\(_, _) -> ("Please enter some more characters.", emptyResult, [], [])))

isEnough :: String -> Bool
isEnough (c:[]) = not (isAlpha c)
isEnough _ = True

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
makeQuery (q, Core i d _ _) = processQuery cfg i d q
                           where
                           cfg = ProcessConfig (FuzzyConfig False True 1.0 []) True 50

-- | Generate a list of modules from a result
genModules :: Result FunctionInfo -> [(String, Int)]
genModules r = reverse $ L.sortBy (compare `on` snd) $ M.toList $ IM.fold collectModules M.empty (docHits r)
  where
  collectModules ((DocInfo d _), _)  modules = maybe modules (\fi -> M.insertWith (+) (takeWhile (/= '.') $ B.toString $ moduleName fi) 1 modules) (custom d)

genPackages :: Result FunctionInfo -> [(String, Int)]
genPackages r = reverse $ L.sortBy (compare `on` snd) $ M.toList $ IM.fold collectPackages M.empty (docHits r)
  where
  collectPackages ((DocInfo d _), _) packages = maybe packages (\fi -> M.insertWith (+) (B.toString $ package fi) 1 packages) (custom d)

-- | Generate an error message in case the query could not be parsed.
genError :: ArrowXml a => a (String, Core) StatusResult
genError = arr $ (\(msg, _) -> (tail $ dropWhile ((/=) ':') msg, emptyResult, [], []))


