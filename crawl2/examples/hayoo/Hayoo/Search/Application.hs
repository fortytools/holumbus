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

module Hayoo.Search.Application
    ( hayooApplication
    , hayooInit
    , Core (..)
    )
where

import Data.Function
import Data.Maybe

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import qualified Data.Text.Encoding as T

import Data.ByteString.Lazy.Char8 (ByteString, pack, fromChunks)

import Network.URI (unEscapeString)

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Unicode

import Text.XHtmlCombinators (render)

import Holumbus.Index.Common

import Holumbus.Query.Language.Grammar
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy

import Holumbus.Utility

import Hack
import Hack.Contrib.Middleware.File
import Hack.Contrib.Middleware.URLMap
import Hack.Contrib.Request (params, path, host)
import Hack.Contrib.Utils (empty_app)

import System.IO (stdout)

import System.Time

import System.Log.Logger
import System.Log.Handler.Simple

import Control.Concurrent  -- For the global MVar

import Hayoo.IndexTypes
import Hayoo.Signature

import Hayoo.Search.Common
import Hayoo.Search.JSON
import Hayoo.Search.HTML
import Hayoo.Search.Parser

import Hayoo.Search.Pages.Template
import Hayoo.Search.Pages.Static

data Core = Core
          { index     :: !  CompactInverted
          , documents :: ! (SmallDocuments FunctionInfo)
          , pkgIndex  :: !  CompactInverted
          , pkgDocs   :: ! (SmallDocuments PackageInfo)
          , template  :: !  Template
          , packRank  :: !  RankTable
          }

-- | Weights for context weighted ranking.
contextWeights          :: [(Context, Score)]
contextWeights          = [ ("name", 0.9)
                          , ("partial", 0.8)
                          , ("module", 0.7)
                          , ("hierarchy", 0.6)
                          , ("package", 0.5)
                          , ("signature", 0.4)
                          , ("description", 0.2)
                          , ("normalized", 0.1)
                          ]

-- | Just an alias with explicit type.
loadIndex       :: FilePath -> IO CompactInverted
loadIndex       = loadFromFile

-- | Just an alias with explicit type.
loadDocuments   :: FilePath -> IO (SmallDocuments FunctionInfo)
loadDocuments   = loadFromFile

-- | Just an alias with explicit type.
loadPkgDocs     :: FilePath -> IO (SmallDocuments PackageInfo)
loadPkgDocs     = loadFromFile

-- | Init Hayoo!
hayooInit :: FilePath -> IO Application
hayooInit ixBase = do
    fdl  <- fileHandler "hayoo.log" INFO
    sdl  <- streamHandler stdout INFO

    updateGlobalLogger rootLoggerName (setHandlers [fdl, sdl])
    updateGlobalLogger rootLoggerName (setLevel INFO)

    idx  <- loadIndex     hayooIndex
    infoM "Hayoo.Main" ("Hayoo index   loaded from file " ++ show hayooIndex)

    doc  <- loadDocuments hayooDocs
    infoM "Hayoo.Main" ("Hayoo docs    loaded from file " ++ show hayooDocs )
    infoM "Hayoo.Main" ("Hayoo docs contains " ++ show (sizeDocs doc) ++ " functions and types")

    pidx <- loadIndex     hackageIndex
    infoM "Hayoo.Main" ("Hackage index loaded from file " ++ show hackageIndex)

    pdoc <- loadPkgDocs   hackageDocs
    infoM "Hayoo.Main" ("Hackage docs  loaded from file " ++ show hackageDocs)
    infoM "Hayoo.Main" ("Hackage docs contains " ++ show (sizeDocs pdoc) ++ " packages")

    prnk <- return $ buildRankTable pdoc
    infoM "Hayoo.Main" ("Hackage package rank table computed")

    tpl  <- return $ makeTemplate (sizeDocs pdoc) (sizeDocs doc)

    midct <- newMVar $
             Core
             { index      = idx
             , documents  = doc
             , pkgIndex   = pidx
             , pkgDocs    = pdoc
             , template   = tpl
             , packRank   = prnk
             }

    return $
           url_map [ ("/hayoo.html", hayooApplication midct)
                   , ("/hayoo.json", hayooApplication midct)
                   , ("/help.html", serveStatic $ tpl help)
                   , ("/about.html", serveStatic $ tpl about)
                   , ("/api.html", serveStatic $ tpl api)
                   ] (file Nothing empty_app)
  where
  hayooIndex      = ixBase ++ "/ix.bin.idx"
  hayooDocs       = ixBase ++ "/ix.bin.doc"
  hackageIndex    = ixBase ++ "/pkg.bin.idx"
  hackageDocs     = ixBase ++ "/pkg.bin.doc"
  serveStatic c _ = return $ Response 
                    { status = 200
                    , headers = [ ("Content-Type", "text/html") ]
                    , body = fromChunks [T.encodeUtf8 $ render c]
                    }

-- | Generate the actual response
hayooApplication :: MVar Core -> Env -> IO Response
hayooApplication midct env      = let p = params env in do
          request <- return $               getValDef p "query"  ""
          start   <- return $ readDef 0    (getValDef p "start"  "")
          static  <- return $ readDef True (getValDef p "static" "")
          json    <- return $ isJson (path env)

          -- Output some information about the request.
          logRequest env

          -- Because index access is read only, the MVar's are just read 
          -- to make them avaliable again.
          idct    <- readMVar midct

          -- Put all information relevant for rendering into a container
          state   <- return $ (request, start, static, (template idct))

          -- If the query is empty, just render an empty page
          resp    <- return $
                     if L.null request
                     then renderEmpty json idct
                     else renderResult state json idct

          -- Determine the mime type of the response
          mime    <- return $ 
                     if json 
                     then "application/json" 
                     else "text/html"

          -- Return the actual response
          return $ Response { status = 200, headers = [ ("Content-Type", mime) ], body = resp }
    where

    -- Just render an empty page/JSON answer
    renderEmpty j idct          = if j
                                  then writeJson
                                  else writeHtml
      where
      writeJson                 = pack $ renderEmptyJson
      writeHtml                 = fromChunks [T.encodeUtf8 $ render $ (template idct) examples]
--      writeHtml     = head $ runLA (writeDocumentToString htmlOptions) (template idct)

    -- Parse the query and generate a result or an error depending on the parse result.
    renderResult :: (String, Int, Bool, Template) -> Bool -> Core  -> ByteString
    renderResult (r, s, i, t) j idct
                                = decode
                                  >>>
                                  parseQuery
                                  >>>
                                  either
                                    (\ msg -> ( tail . dropWhile ((/=) ':') $ msg
                                              , emptyResult, emptyResult, [], []
                                              )
                                    )
                                    (genResult idct)
                                  >>>
                                  ( if j
                                    then pack . renderJson
                                    else writeHtml (RenderState r s i)
                                  )
                                  $ r
      where
      writeHtml rs              = filterStatusResult r
                                  >>>
                                  arr (applyTemplate rs)
{-
																	runLA
                                  ( xpickleVal (xpStatusResult ps)
                                    >>>
                                    applyTemplate ps
                                    >>>
                                    ( writeDocumentToString $
                                      (a_no_xml_pi, if psStatic ps then v_0 else v_1) : htmlOptions
                                    )
                                  )
																	
                                  >>>
                                  head
-}
      applyTemplate rs sr       = fromChunks [T.encodeUtf8 markup]
                                  where
                                  markup = let rr = result rs sr in 
                                           if rsStatic rs then render $ t rr else render $ rr

{-
		applyTemplate rs            = if rsStatic rs
                                  then ( insertTreeTemplate (constA $ psTemplate ps) [ hasAttrValue "id" (== "result") :-> this ]
                                         >>>
                                         staticSubstitutions ps
                                         >>>
                                         addXHtmlDoctypeStrict
                                       )
                                  else arr id
-}
    -- Check requested path for JSON
    isJson f                    = extension f == "json"
{-
    -- Default HTML render options
    htmlOptions                 = [(a_output_encoding, utf8), (a_indent,v_1), (a_output_xhtml, v_1)]
-}
-- Read or use default value
readDef                         :: Read a => a -> String -> a
readDef d                       = fromMaybe d . readM

-- | Determine the file name extension of a path
extension                       :: String -> String
extension fn                    = go (reverse fn) ""
  where
  go []      _                  = ""
  go ('.':_) ext                = ext
  go (x:s)   ext                = go s (x:ext)

getValDef                       :: [(String,String)] -> String -> String -> String
getValDef l k d                 = fromMaybe d (lookup k l)

{-
staticSubstitutions             :: ArrowXml a => PickleState -> a XmlTree XmlTree
staticSubstitutions ps          = processTopDown setQuery
  where
  setQuery                      = processAttrl ( changeAttrValue (\_ -> urlDecode $ psQuery ps)
                                                 `when`
                                                 hasName "value"
                                               )
                                  `when`
                                  ( isElem
                                    >>>
                                    hasAttrValue "id" (== "querytext")
                                  )
-}
-- | Enable handling of parse errors from 'read'.
readM   :: (Read a, Monad m) => String -> m a
readM s = case reads s of
                                  [(x, "")] -> return x
                                  _         -> fail "No parse"
{-
-- | Proper URL decoding including substitution of "the annoying +" (tm)
urlDecode :: String -> String
urlDecode = unEscapeString . replaceElem '+' ' '
-}
-- | Decode any URI encoded entities and transform to unicode.
decode :: String -> String
decode = fst . utf8ToUnicode . unEscapeString   -- with urlDecode the + disapears
{-
replaceElem     :: Eq a => a -> a -> [a] -> [a]
replaceElem x y = map (\z -> if z == x then y else z)
-}
-- | Perform some postprocessing on the status and the result.
filterStatusResult :: String -> StatusResult -> StatusResult
filterStatusResult q (s, r@(Result dh wh), h, m, p)
    = (s, filteredResult, h, m, p)
  where
  filteredResult
      | isSignature q = r
      | otherwise     = Result dh (M.filterWithKey (\x _y -> not . isSignature $ x) wh)

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

hayooPkgRanking :: RankTable -> DocId -> DocInfo PackageInfo -> DocContextHits -> Score
hayooPkgRanking rt _ di _ = maybe 1.0 (flip lookupRankTable rt . p_name) (custom $ document di)

-- | Customized Hayoo! ranking function for functions. Preferres exact matches and matches in Prelude and base.
hayooFctRanking                 :: RankTable -> [(Context, Score)] -> [String] -> DocId -> DocInfo FunctionInfo -> DocContextHits -> Score
hayooFctRanking rt ws ts _ di dch
                        = baseScore
                          * factModule
                          * factPackage
                          * factPrelude
                          * factExactMatch
  where
  fctInfo               = custom $ document di

  baseScore             = M.foldWithKey calcWeightedScore 0.0 dch

  factExactMatch        = L.foldl' (\r t -> t == (title $ document di) || r) False
                          >>> fromEnum
                          >>> (+ 1)
                          >>> fromIntegral
                          >>> (* 4.0)
                          $ ts

  factPrelude           = fmap ( moduleName
                                 >>> (== "Prelude")
                                 >>> fromEnum
                                 >>> (+ 1)
                                 >>> fromIntegral
                                 >>> (* 2.0)
                               )
                          >>> fromMaybe 1.0
                          $ fctInfo

  factPackage           = fmap ( package
                                 >>> flip lookupRankTable rt
                               )
                          >>> fromMaybe 1.0
                          $ fctInfo

  factModule            = fmap ( moduleName
                                 >>> split "."
                                 >>> length
                                 >>> fromIntegral
                                 >>> (1.0 /)
                               )
                          >>> fromMaybe 1.0
                          $ fctInfo

  calcWeightedScore     :: Context -> DocWordHits -> Score -> Score
  calcWeightedScore c h r
                        = maybe r (\w -> r + ((w / mw) * count)) (lookupWeight ws)
    where
    count               = fromIntegral $ M.fold ((+) . IS.size) 0 h
    mw                  = snd $ L.maximumBy (compare `on` snd) ws
    lookupWeight []     = Nothing
    lookupWeight (x:xs) = if fst x == c then
                            if snd x /= 0.0
                            then Just (snd x)
                            else Nothing
                          else lookupWeight xs

genResult ::  Core -> Query -> StatusResult
genResult idc q
      = let (fctRes, pkgRes) = curry makeQuery q idc in
        let (fctCfg, pkgCfg) = (RankConfig (hayooFctRanking (packRank idc) contextWeights (extractTerms q)) wordRankByCount, RankConfig (hayooPkgRanking (packRank idc)) wordRankByCount) in
        let (fctRnk, pkgRnk) = (rank fctCfg fctRes, rank pkgCfg pkgRes) in
        (msgSuccess fctRnk pkgRnk, fctRnk, pkgRnk, genModules fctRnk, genPackages fctRnk) -- Include a success message in the status

-- | Generate a success status response from a query result.
msgSuccess              :: Result FunctionInfo -> Result PackageInfo -> String
msgSuccess fr pr        = if sd + sp == 0
                          then "Nothing found yet."
                          else "Found " ++ (show sd) ++ " " ++ ds ++ ", " ++ (show sp) ++ " " ++ ps ++ " and " ++ (show sw) ++ " " ++ cs ++ "."
    where
    sd                  = sizeDocHits fr
    sp      = sizeDocHits pr
    sw                  = sizeWordHits fr + sizeWordHits pr
    ds                  = if sd == 1 then "function" else "functions"
    ps                  = if sp == 1 then "package" else "packages"
    cs                  = if sw == 1 then "completion" else "completions"

-- | This is where the magic happens! This helper function really calls the
-- processing function which executes the query.
makeQuery               :: (Query, Core) -> (Result FunctionInfo, Result PackageInfo)
makeQuery (q, c)        = (processQuery cfg (index c) (documents c) q, processQuery cfg (pkgIndex c) (pkgDocs c) q)
    where
    cfg                 = ProcessConfig
                          { fuzzyConfig   = FuzzyConfig False True 1.0 []
                          , optimizeQuery = True
                          , wordLimit     = 50
                          , docLimit      = 500
                          }

-- | Generate a list of modules from a result
genModules              :: Result FunctionInfo -> [(String, Int)]
genModules r            = reverse $
                          L.sortBy (compare `on` snd) $
                          M.toList $
                          IM.fold collectModules M.empty (docHits r)
  where
  collectModules ((DocInfo d _), _)  modules
                        = maybe modules (\fi -> M.insertWith (+) (takeWhile (/= '.') . moduleName $ fi) 1 modules) $
                          custom d

genPackages             :: Result FunctionInfo -> [(String, Int)]
genPackages r           = reverse $
                          L.sortBy (compare `on` snd) $
                          M.toList $
                          IM.fold collectPackages M.empty (docHits r)
  where
  collectPackages ((DocInfo d _), _) packages
                        = maybe packages (\fi -> M.insertWith (+) (package fi) 1 packages) $
                          custom d

-- ----------------------------------------------------------------------------
