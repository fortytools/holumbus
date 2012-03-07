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

module Hayoo.Search.EvalSearch
    ( parseQuery
    , genResult'
    , emptyRes

    , isJson
    , renderJson
    , renderEmptyJson

    , filterStatusResult

    , decode

    , readDef
    , getValDef

    , loadIndex
    , loadDocuments
    , loadPkgIndex
    , loadPkgDocs

#if hayooSnap4
    , Core(..)
    , examples
    , genResult
    , renderEmpty
    , renderResult
#endif
    )
where

import Data.Function
import Data.Maybe

import qualified Data.List              as L
import qualified Data.Map               as M

import Data.String.Unicode

import Holumbus.Index.Common

import Holumbus.Query.Language.Grammar
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy

import Holumbus.Utility

import Hayoo.IndexTypes
import Hayoo.Signature

import Hayoo.Search.Common
import Hayoo.Search.JSON
import Hayoo.Search.Parser

import Network.URI                      ( unEscapeString )

import System.FilePath                  ( takeExtension )

import Text.XML.HXT.Core

#if hayooSnap4
import Data.ByteString.Lazy.Char8       ( ByteString
                                        , pack
                                        , fromChunks
                                        )
import qualified Data.Text.Encoding     as T

import Hayoo.Search.HTML
import Hayoo.Search.Pages.Template
import Hayoo.Search.Pages.Static

import qualified
       Text.XHtmlCombinators            as X
#endif

-- ------------------------------------------------------------

#if hayooSnap4
data Core = Core
          { index     :: ! HayooFctIndex
          , documents :: ! HayooFctDocuments
          , pkgIndex  :: ! HayooPkgIndex
          , pkgDocs   :: ! HayooPkgDocuments
          , template  :: ! Template
          , packRank  :: ! RankTable
          }

-- | Just render an empty page/JSON answer

renderEmpty                     :: Bool -> Core -> ByteString
renderEmpty j idct
    | j                         = writeJson
    | otherwise                 = writeHtml
    where
    writeJson                   = pack $ renderEmptyJson
    writeHtml                   = fromChunks [T.encodeUtf8 $ X.render $ (template idct) examples]

-- | Parse the query and generate a result or an error depending on the parse result.

renderResult :: (String, Int, Bool, Template) -> Bool -> Core  -> ByteString
renderResult (r, s, i, t) j idct
                        = decode
                          >>>
                          parseQuery
                          >>>
                          either emptyRes (genResult idct)
                          >>>
                          ( if j
                            then pack . renderJson
                            else writeHtml (RenderState r s i)
                          )
                          $ r
      where
      writeHtml rs      = filterStatusResult r
                          >>>
                          arr (applyTemplate rs)
      applyTemplate rs sr
                        = fromChunks [T.encodeUtf8 markup]
          where
          markup        = let rr = result rs sr in 
                          if rsStatic rs
                          then X.render $ t rr
                          else X.render $ rr

genResult ::  Core -> Query -> StatusResult
genResult c q
    = genResult' q (index c) (documents c) (pkgIndex c) (pkgDocs c) (packRank c)

#endif

-- ------------------------------------------------------------

-- | Weights for context weighted ranking.
contextWeights          :: [(Context, Score)]
contextWeights          = [ ("name",        0.9)
                          , ("partial",     0.8)
                          , ("module",      0.7)
                          , ("hierarchy",   0.6)
                          , ("package",     0.5)
                          , ("signature",   0.4)
                          , ("description", 0.2)
                          , ("normalized",  0.1)
                          ]

-- ------------------------------------------------------------

-- | Just an alias with explicit type.
loadIndex       :: FilePath -> IO HayooFctIndex
loadIndex       = loadFromFile

loadPkgIndex    :: FilePath -> IO HayooPkgIndex
loadPkgIndex    = loadFromFile

-- | Just an alias with explicit type.
loadDocuments   :: FilePath -> IO HayooFctDocuments
loadDocuments   = loadFromFile

-- | Just an alias with explicit type.
loadPkgDocs     :: FilePath -> IO (HayooPkgDocuments)
loadPkgDocs     = loadFromFile

-- ------------------------------------------------------------

-- Read or use default value
readDef                         :: Read a => a -> String -> a
readDef d                       = fromMaybe d . readM

getValDef                       :: [(String,String)] -> String -> String -> String
getValDef l k d                 = fromMaybe d (lookup k l)

-- | Enable handling of parse errors from 'read'.
readM                           :: (Read a, Monad m) => String -> m a
readM s                         = case reads s of
                                  [(x, "")] -> return x
                                  _         -> fail "No parse"

-- ------------------------------------------------------------

-- | Decode any URI encoded entities and transform to unicode.
decode                          :: String -> String
decode                          = fst . utf8ToUnicode . unEscapeString   -- with urlDecode the + disapears

-- | Perform some postprocessing on the status and the result.
filterStatusResult              :: String -> StatusResult -> StatusResult
filterStatusResult q (s, r@(Result dh wh), h, m, p)
                                = (s, filteredResult, h, m, p)
  where
  filteredResult
      | isSignature q           = r
      | otherwise               = Result dh (M.filterWithKey (\x _y -> not . isSignature $ x) wh)

-- Check requested path for JSON
isJson                  :: FilePath -> Bool
isJson f                = takeExtension f == ".json"

-- ------------------------------------------------------------

hayooPkgRanking         :: RankTable -> DocId -> DocInfo PackageInfo -> DocContextHits -> Score
hayooPkgRanking rt _ di _
                        = maybe 1.0 (flip lookupRankTable rt . p_name) (custom $ document di)

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

  baseScore             = M.foldrWithKey calcWeightedScore 0.0 dch

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
    count               = fromIntegral $ M.fold ((+) . sizePos) 0 h
    mw                  = snd $ L.maximumBy (compare `on` snd) ws
    lookupWeight []     = Nothing
    lookupWeight (x:xs) = if fst x == c then
                            if snd x /= 0.0
                            then Just (snd x)
                            else Nothing
                          else lookupWeight xs

emptyRes                :: String -> StatusResult
emptyRes msg            = ( tail . dropWhile ((/=) ':') $ msg
                          , emptyResult
                          , emptyResult
                          , []
                          , []
                          )

genResult' :: Query
           -> HayooFctIndex -> HayooFctDocuments
           -> HayooPkgIndex -> HayooPkgDocuments
           -> RankTable
           -> StatusResult
genResult' q fx fd px pd pr
      = let (fctRes, pkgRes) = makeQuery q fx fd px pd in
        let (fctCfg, pkgCfg) = ( RankConfig (hayooFctRanking pr contextWeights (extractTerms q)) wordRankByCount
                               , RankConfig (hayooPkgRanking pr)                                 wordRankByCount
                               ) in
        let (fctRnk, pkgRnk) = ( rank fctCfg fctRes
                               , rank pkgCfg pkgRes
                               ) in
        ( msgSuccess fctRnk pkgRnk
        , fctRnk
        , pkgRnk
        , genModules fctRnk
        , genPackages fctRnk
        ) -- Include a success message in the status

-- | Generate a success status response from a query result.
msgSuccess              :: Result FunctionInfo -> Result PackageInfo -> String
msgSuccess fr pr        = if sd + sp == 0
                          then "Nothing found yet."
                          else "Found " ++ (show sd) ++ " " ++ ds ++ ", " ++ (show sp) ++ " " ++ ps ++ " and " ++ (show sw) ++ " " ++ cs ++ "."
    where
    sd                  = sizeDocHits fr
    sp                  = sizeDocHits pr
    sw                  = sizeWordHits fr + sizeWordHits pr
    ds                  = if sd == 1 then "function" else "functions"
    ps                  = if sp == 1 then "package" else "packages"
    cs                  = if sw == 1 then "completion" else "completions"

-- | This is where the magic happens! This helper function really calls the
-- processing function which executes the query.

makeQuery :: Query ->
             HayooFctIndex -> HayooFctDocuments ->
             HayooPkgIndex -> HayooPkgDocuments ->
             (Result FunctionInfo, Result PackageInfo)
makeQuery q index_c documents_c pkgIndex_c pkgDocs_c
    = ( processQuery cfg (index_c) (documents_c) q
      , processQuery cfg (pkgIndex_c) (pkgDocs_c) q
      )
    where
      cfg = ProcessConfig
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
                          foldDocIdMap collectModules M.empty (docHits r)
  where
  collectModules ((DocInfo d _), _)  modules
                        = maybe modules (\fi -> M.insertWith (+) (takeWhile (/= '.') . moduleName $ fi) 1 modules) $
                          custom d

genPackages             :: Result FunctionInfo -> [(String, Int)]
genPackages r           = reverse $
                          L.sortBy (compare `on` snd) $
                          M.toList $
                          foldDocIdMap collectPackages M.empty (docHits r)
  where
  collectPackages ((DocInfo d _), _) packages
                        = maybe packages (\fi -> M.insertWith (+) (package fi) 1 packages) $
                          custom d

-- ----------------------------------------------------------------------------
