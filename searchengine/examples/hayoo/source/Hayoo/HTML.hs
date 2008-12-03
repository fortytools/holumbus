-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.HTML
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

{-# OPTIONS -fglasgow-exts -fno-warn-type-defaults #-}

module Hayoo.HTML 
  ( PickleState (..)
  , xpStatusResult
  )
where

import Control.Monad

import Data.Function

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.UTF8 as B

import System.IO.Unsafe

import Text.XML.HXT.Arrow

import Holumbus.Index.Common
import Holumbus.Index.Cache

import Holumbus.Query.Result

import Hayoo.Common


data PickleState = PickleState
  { psStart :: Int
  , psCache :: Cache
  }

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
xpElemClass :: String -> String -> PU a -> PU a
xpElemClass e c p = xpElem e (xpAddFixedAttr "class" c p)

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
  docToHtml (i, (DocInfo (Document t u (Just (FunctionInfo m s p l))) _, _)) = (((modLink u, B.toString m), (u, t), B.toString s), ((pkgLink $ B.toString p, B.toString p), (getDesc i, liftM B.toString $ l)))
    where
    modLink = takeWhile ((/=) '#')
    pkgLink "gtk2hs" = "http://www.haskell.org/gtk2hs"
    pkgLink "base" = "http://hackage.haskell.org/packages/archive/base/4.0.0.0/doc/html/"
    pkgLink p' = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ p'
    getDesc = unsafePerformIO . getDocText c "description" 
  xpQualified = xpElem "tr" $ xpClass "function" $ xpTriple xpModule xpFunction xpSignature
    where
    xpModule = xpCell "module" $ xpPair (xpElemClass "a" "module" $ xpAttr "href" $ xpText) (xpAppend "." $ xpText)
    xpFunction = xpCell "function" $ xpPair (xpElemClass "a" "function" $ xpAttr "href" $ xpText) xpText
    xpSignature = xpCell "signature" $ xpPrepend ":: " xpSigDecl
  xpAdditional = xpElemClass "tr" "description" $ xpPair xpPackage xpDescSource
    where
    xpPackage = xpCell "package" $ xpPair (xpElemClass "a" "package" $ xpAttr "href" $ xpText) xpText
    xpDescSource = xpCell "description" $ xpAddFixedAttr "colspan" "2" $ xpPair xpDescription xpSource
    xpDescription = xpWrap (undefined, limitDescription) (xpElemClass "span" "description" $ xpText)
    xpSource = xpOption $ (xpElemClass "span" "source" $ xpElemClass "a" "source" $ xpAppend "Source" $ xpAttr "href" $ xpText)
    limitDescription = maybe "No description. " (\d -> if length d > 100 then (take 100 d) ++ "... " else d ++ " ")

data Signature = Signature String
               | Declaration String

xpSigDecl :: PU String
xpSigDecl = xpWrap (undefined, makeSignature) (xpAlt tag [xpSig, xpDecl])
  where
  xpSig = xpWrap (Signature, \(Signature s) -> s) xpText
  xpDecl = xpWrap (Declaration, \(Declaration s) -> s) (xpElemClass "span" "declaration" $ xpText)
  tag (Signature _) = 0
  tag (Declaration _) = 1
  makeSignature s = if (s == "data") || (s == "type") || (s == "newtype") || (s == "class")
                    then Declaration s
                    else Signature s

xpCell :: String -> PU a -> PU a
xpCell c p = xpElem "td" $ xpClass c $ p

xpWordHitsHtml :: PU (Score, WordHits)
xpWordHitsHtml = xpDivId "words" $ xpElemClass "p" "cloud" $ xpWrap (fromListSorted, toListSorted) (xpList xpWordHitHtml)
  where
  fromListSorted _ = (0.0, M.empty)
  toListSorted (s, wh) = map (\a -> (s, a)) $ L.sortBy (compare `on` fst) $ M.toList wh -- Sort by word
  xpWordHitHtml = xpWrap (wordFromHtml, wordToHtml) (xpWordHtml)
    where
    wordToHtml (m, (w, (WordInfo ts s, _))) = ((head ts, w), ((s, m), w))
    wordFromHtml ((t, _), ((s, m), w)) = (m, (w, (WordInfo [t] s, M.empty)))
    xpWordHtml = xpAppend " " $ xpElemClass "a" "cloud" $ xpPair xpCloudLink xpScore

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
