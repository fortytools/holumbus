-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Search.Pages.Template
  Copyright  : Copyright (C) 2010 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The main Hayoo! template.
-}

-- ----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Search.Pages.Template (Template, makeTemplate) where

import qualified Data.List as L
import qualified Data.Text as T

import Control.Category

import Text.XHtmlCombinators
import qualified Text.XHtmlCombinators.Attributes as A

type Template = XHtml FlowContent -> XHtml Page

-- | Make a Hayoo! template from the given package//function statistics.
makeTemplate :: Int -> Int -> Template
makeTemplate np nf template = html True $ do
  makeHead
  makeBody np nf template

makeHead :: XHtml TopLevelContent
makeHead = head_ $ do
  meta' "text/html; charset=UTF-8" [A.httpEquiv "content-type"]
  meta' "Haskell, API, Search, Hackage, Functions, Types, Packages" [A.name "keywords"]
  meta' "A Haskell API search engine with find-as-you-type and suggestions. Searches for function and type defintions in all Haskell packages from Hackage." [A.name "description", A.lang "en"]

  link' [A.type_ "image/ico", A.href "hayoo/favicon.ico", A.rel "Shortcut icon"]
  link' [A.type_ "text/css", A.href "hayoo/hayoo.css", A.rel "stylesheet"]
  link' [A.type_ "application/opensearchdescription+xml", A.href "hayoo/opensearch.xml", A.rel "search"]

  title "Hayoo! - Haskell API Search"

  script' "text/javascript" [A.src "hayoo/prototype.js"] "&nbsp;"
  script' "text/javascript" [A.src "hayoo/hayoo.js"] "&nbsp;"

  -- Piwik tracking stuff
  script "text/javascript" 
    "var pkBaseURL = (('https:' == document.location.protocol) ? 'https://piwik.hayoo.info/' : 'http://piwik.hayoo.info/'); \
    \document.write(unescape(\"%3Cscript src='\" + pkBaseURL + \"piwik.js' type='text/javascript'%3E%3C/script%3E\"));"
  script "text/javascript"
    "try {\
      \var piwikTracker = Piwik.getTracker(pkBaseURL + \"piwik.php\", 3);\
      \piwikTracker.trackPageView();\
      \piwikTracker.enableLinkTracking();\
    \} catch( err ) {}"

makeInfo :: XHtml FlowContent
makeInfo = div' [A.id_ "info"] $ do
  a' [A.href "help.html"] $ text "Help"
  text " | "
  a' [A.href "about.html"] $ text "About"
  text " | "
  a' [A.href "api.html"] $ text "API"
  text " | "
  a' [A.href "/blog"] $ text "Blog"
  text " | "
  a' [A.href "http://hackage.haskell.org"] $ text "Hackage"
  text " | "
  a' [A.href "http://www.haskell.org"] $ text "Haskell"

makeQuery :: Int -> Int -> XHtml FlowContent
makeQuery np nf = div' [A.id_ "query"] $ do
  div' [A.id_ "logo"] $ a' [A.href "hayoo.html"] $ img' "" "" [A.src "hayoo/hayoo.png", A.alt "Hayoo! logo", A.class_ "logo"]
  form' "hayoo.html" [A.id_ "queryform", A.attr "onsubmit" "return forceProcessQuery()", A.method "get"] $ div' [A.id_ "queryinterface"] $ do
    input' [A.id_ "querytext", A.type_ "text", A.attr "onkeyup" "tryProcessQuery()", A.name "query", A.attr "autocomplete" "off", A.value ""]
    input' [A.id_ "querybutton", A.type_ "submit", A.value "Search"]
    img' "" "" [A.src "hayoo/loader.gif", A.alt "Throbber", A.id_ "throbber", A.style "display:none;"]
    div' [A.class_ "stats"] $ do
      text $ T.concat ["Concurrently search more than ", T.pack $ showCnt np, " packages and more than ", T.pack $ showCnt nf, " functions!"]

makeCredits :: XHtml FlowContent
makeCredits = div' [A.id_ "credits"] $ do
  div' [A.id_ "powered"] $ do
    div' [A.id_ "libs"] $ do
      text "Powered by " 
      a' [A.class_ "credits", A.href "http://www.haskell.org"] $ text "Haskell"
      text ", "
      a' [A.class_ "credits", A.href "http://www.fh-wedel.de/~si/HXmlToolbox/"] $ text "HXT"
      text " and "
    a' [A.href "http://holumbus.fh-wedel.de"] $ img' "" "" [A.src "hayoo/holumbus.png", A.alt "Holumbus logo", A.class_ "logo"]

  div' [A.id_ "authors"] $ do
    div' [A.id_ "feedback"] $ do
      text "Please send any feedback to "
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
    div' [A.id_ "copyright"] $ do
      text "Hayoo! beta 2.2 © 2010 "
      span' [A.class_ "author"] $ a' [A.href "http://tbh.github.com"] $ text "Timo B. Hübel"
      text ", "
      span' [A.class_ "author"] $ text "Sebastian M. Gauck"
      text " & "
      span' [A.class_ "author"] $ text "Uwe Schmidt"

makeBody :: Int -> Int -> XHtml FlowContent -> XHtml TopLevelContent
makeBody np nf content = body $ do
  div' [A.id_ "container"] $ do
    makeInfo
    makeQuery np nf
    content
    makeCredits
    makeTracking

makeTracking :: XHtml FlowContent
makeTracking = 
  noscript $ do
    p $ do
      img' "" "" [A.src "http://piwik.hayoo.info/piwik.php?idsite=3", A.alt "", A.style "border:0"]

showCnt         :: Int -> String
showCnt         = show >>> fmtCnt
    where
    fmtCnt      = reverse >>> insDot >>> reverse
    insDot s
        | L.null y      = s
        | otherwise     = x ++ "." ++ insDot y
        where
        (x , y) = splitAt 3 s


