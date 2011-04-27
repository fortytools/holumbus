-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Search.HTML
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The conversion functions for generating the HTML code of an Hayoo!
  search result.
 
-}

-- ----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Search.HTML 
    ( RenderState (..)
    , result
    )
where

import           Data.Text (Text, pack)
import           Data.Char (toLower, isSpace)
import           Data.Maybe
import           Data.Function
import qualified Data.IntMap  as IM
import qualified Data.List    as L
import qualified Data.Map     as M

import           Text.XHtmlCombinators
import qualified Text.XHtmlCombinators.Attributes as A

import           Holumbus.Utility hiding (escape)
import           Holumbus.Index.Common

import           Holumbus.Query.Result

import           Hayoo.IndexTypes
import           Hayoo.Search.Common

data RenderState = RenderState
                 { rsQuery    :: String
                 , rsStart    :: Int
                 , rsStatic   :: Bool
                 }

pageLimit :: Int
pageLimit = 10

maxWordHits :: Int
maxWordHits = 75

staticRoot :: String
staticRoot = "hayoo.html"

text' :: (Functor t, Monad t, CData c) => String -> XHtmlT t c
text' = text . pack

onclick :: Text -> Attr
onclick = A.attr "onclick"

-- | Render the whole Hayoo! search result.
result :: RenderState -> StatusResult -> XHtml FlowContent
result rs (sm, rf, rp, tm, tp) = div' [A.id_ "result"] $ do
  div' [A.id_ "status"] $ text' sm  
  packageResults rs rp
  topLists rs tm tp
  functionResults rs rf
  div' [A.class_ "clear"] $ empty
  pager rs $ max (sizeDocHits rf) (sizeDocHits rp)

-- | The aggregation lists.
topLists :: RenderState -> [(String, Int)] -> [(String, Int)] -> XHtml FlowContent
topLists _ [] [] = empty
topLists rs tm tp = div' [A.id_ "aggregation"] $ do
  moduleList rs tm
  packageList rs tp

-- | The aggregated module list.
moduleList :: RenderState -> [(String, Int)] -> XHtml FlowContent
moduleList _ [] = empty
moduleList rs tm = div' [A.id_ "modules"] $ do
  div' [A.class_ "headline"] $ text "Top 15 Modules"
  mapM_ topModule (take 15 tm)
  where
  topModule (m, c) = div' [A.class_ "rootModule"] $ do
    a' [A.class_ "rootModuleName", A.href staticLink, onclick dynamicLink] $ text' m
    text " "
    span' [A.class_ "rootModuleCount"] $ text' $ show c
      where
      staticLink = pack $ staticRoot ++ "?query=" ++ (rsQuery rs) ++ "%20module%3A" ++ m
      dynamicLink = pack $ "addToQuery('module:" ++ m ++ "'); return false;"

-- | The aggregated package list.
packageList :: RenderState -> [(String, Int)] -> XHtml FlowContent
packageList _ [] = empty
packageList rs tp = div' [A.id_ "packages"] $ do
  div' [A.class_ "headline"] $ text "Top 15 Packages"
  mapM_ topPackage (take 15 tp)
  where
  topPackage (k, c) = div' [A.class_ "package"] $ do
    a' [A.class_ "packageLink", A.href staticLink, onclick dynamicLink] $ text' k
    text " "
    span' [A.class_ "packageCount"] $ text' $ show c
      where
      staticLink = pack $ staticRoot ++ "?query=" ++ (rsQuery rs) ++ "%20package%3A" ++ k
      dynamicLink = pack $ "addToQuery('package:" ++ k ++ "'); return false;"

-- | The Hackage hits (i.e. packages)
packageResults :: RenderState -> (Result PackageInfo) -> XHtml FlowContent
packageResults rs rp = let pl = extractPackages rp in 
  if L.null pl then empty else div' [A.id_ "hackage"] $ do 
    mapM_ packageInfo pl
  where
  packageInfo k = div' [A.class_ "package"] $ do
    div' [A.class_ "category"] $ mapM_ packageCategory (split "," $ p_category k)
    div' [A.class_ "name"] $ do
      a' [A.class_ "name", A.href $ pack $ "http://hackage.haskell.org/package/" ++ (p_name k)] $ 
                                text' (p_name k)
      span' [A.class_ "synopsis"] $ text' (" " ++ (p_synopsis k))
    div' [A.class_ "description"] $ text' (p_description k)
    div' [A.class_ "author"] $ text' (p_author k)
  packageCategory c = a' [A.class_ "category", A.href $ pack $ "http://hackage.haskell.org/packages/archive/pkg-list.html#cat:" ++ map toLower c] $ text' c
  extractPackages = take pageLimit . drop (rsStart rs) . 
                    (mapMaybe (\(_, (di, _)) -> custom $ document di)) . 
                    reverse . L.sortBy (compare `on` (docScore . fst . snd)) . IM.toList . docHits

-- | The function hits.
functionResults :: RenderState -> (Result FunctionInfo) -> XHtml FlowContent
functionResults rs rf = do
  div' [A.id_ "words"] $ mapM_ (wordInfo rs $ maxScoreWordHits rf) (toListSortedWords $ wordHits rf)
  div' [A.id_ "documents"] $ table $ mapM_ functionInfo (toListSortedDocs $ docHits rf)
    where
                notSignature (_, (_, wch)) = M.keys wch /= ["signature"]
                toListSortedDocs = take pageLimit . drop (rsStart rs) . reverse . 
                        L.sortBy (compare `on` (docScore . fst . snd)) . IM.toList
                toListSortedWords = L.sortBy (compare `on` fst) . take maxWordHits . 
                        L.sortBy (compare `on` (wordScore .fst . snd)) . filter notSignature . M.toList

-- | Render the information about a function (module w/ link, function name, signature, ...)
functionInfo :: (DocId, (DocInfo FunctionInfo, DocContextHits)) -> XHtml Table1Content
functionInfo (_, (DocInfo (Document _ _ Nothing) _, _)) = error "Expecting custom document info"
functionInfo (_, (DocInfo (Document t u (Just fi)) r, _)) = tbody $ do
        tr' [A.class_ "function"] $ do
                td' [A.class_ "module"] $ do
                        a' [A.class_ "module", A.href $ pack (modLink u)] $ text' (moduleName fi ++ ".")
                td' [A.class_ "name"] $ do
                        a' [A.class_ "function", A.href $ pack u, A.title $ pack ("Score: " ++ show r)] $ text' t
                td' [A.class_ "signature"] $ do
                        sigDecl (signature fi)
        tr' [A.class_ "details"] $ do
                td' [A.class_ "package"] $ do
                        a' [A.class_ "package", A.href $ pack (pkgLink $ package fi)] $ text' (package fi)
                td' [A.class_ "description", A.colspan 2] $ div_ $ do
                        a' [A.class_ "toggleFold", onclick "toggleFold(this);"] $ empty
                        div' [A.class_ "description"] $ let f = fctDescr fi in
                                if L.null f then text "No description." else text' f
                        let c = sourceURI fi in if L.null c then empty else span' [A.class_ "source"] $ do
                                a' [A.class_ "source", A.href $ pack c] $ text "Source"
        where
        modLink = takeWhile ((/=) '#')
        pkgLink = (++) "http://hackage.haskell.org/package/"
        sigDecl s'
                | s' `elem` ["data", "type", "newtype", "class", "module"] = span' [A.class_ "declaration"] $ text' s
                | otherwise = text' $ replace "->" " -> " s
                where
                s = ":: " ++ stringTrim s'

-- | Render a word in the cloud of suggestions
wordInfo :: RenderState -> Score -> (Word, (WordInfo, WordContextHits)) -> XHtml FlowContent
wordInfo rs m (w, (WordInfo ts s, c)) = do
        a' [A.class_ "cloud", A.href staticLink, onclick dynamicLink, A.title $ pack origin] $ do
                span' [A.class_ $ pack $ "cloud" ++ (show ((round $ weightScore 1 9 m s)::Int))] $ text' w
        text " "
        where
        origin = join ", " $ M.keys c
        dynamicLink = pack $ "replaceInfQuery('" ++ escape t ++ "','" ++ escape w ++ "'); return false;"
        staticLink = pack $  staticRoot ++ "?query=" ++ (replace t w qu) ++ "&start=" ++ (show st)
        weightScore mi ma to v  = ma - ((to - v) / to) * (ma - mi)
        escape []      = []
        escape (x:xs)  = if x == '\'' then "\\'" ++ escape xs else x : (escape xs)
        t = head ts
        qu = rsQuery rs
        st = rsStart rs

pager :: RenderState -> Int -> XHtml FlowContent
pager rs m = if m <= 0 || m < pageLimit then empty else div'[A.id_ "pager"] $ do
                nav "previous" "<" (_prevPage pg)
                mapM_ page (_predPages pg)
                span' [A.class_ "current"] $ text' $ show $  _currPage pg
                mapM_ page (_succPages pg)
                nav "next" ">" (_nextPage pg)
        where
        pg = makePager (rsStart rs) pageLimit m
        nav :: Text -> String -> Maybe Int -> XHtml FlowContent
        nav _ _ Nothing = empty
        nav c t (Just v) = a' [A.class_ c, A.href $ statLink v, onclick $ dynLink v] $ text' t
        page (v, t)  = a' [A.class_ "page", A.href $ statLink v, onclick $ dynLink v] $ text' $ show t
        dynLink v = pack $ "showPage(" ++ show v ++ "); return false;"
        statLink v = pack $ staticRoot ++ "?query=" ++ (rsQuery rs) ++ "&start=" ++ show v

data Pager  = Pager 
              { _prevPage  :: Maybe Int -- == last predPages
              , _predPages :: [(Int, Int)]
              , _currPage  :: Int
              , _succPages :: [(Int, Int)]
              , _nextPage  :: Maybe Int -- == head succPages
              }

-- Start element (counting from zero), elements per page, total number of elements.
makePager :: Int -> Int -> Int -> Pager
makePager s pg n = Pager pv (drop (length pd - 10) pd) (length pd + 1) (take 10 sc) nt
  where
  pv = if s < pg then Nothing else Just (s - pg)
  nt = if s + pg >= n then Nothing else Just (s + pg)
  pd = map (\x -> (x, x `div` pg + 1)) $ genPred s []
    where
    genPred rp tp = let np = rp - pg in
                                  if np < 0
                                  then tp
                                  else genPred np (np:tp)
  sc = map (\x -> (x, x `div` pg + 1)) $ genSucc s []
    where
    genSucc rs ts = let ns = rs + pg in
                                  if ns >= n
                                  then ts
                                  else genSucc ns (ts ++ [ns])

-- | Replace a given list with another list in a list.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = L.intercalate new . split old $ l

-- | Remove leading and trailing whitespace using isSpace.
stringTrim :: String -> String
stringTrim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

