{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Search.Pages.Static
  Copyright  : Copyright (C) 2010 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The main Hayoo! template.
-}

-- ----------------------------------------------------------------------------

module Hayoo.Search.Pages.Static (help, about, api, examples) where

import Data.Text (Text)

import Text.XHtmlCombinators
import qualified Text.XHtmlCombinators.Attributes as A

examples :: XHtml FlowContent
examples = div' [A.id_ "result"] $ do
  div' [A.id_ "status"] $ text "Enter some search terms above to start a search."
  div' [A.id_ "words"] $ text " "
  div' [A.id_ "documents"] $ div' [A.id_ "examples"] $ do
    text "Hayoo! will search all packages from "
    a' [A.href "http://hackage.haskell.org"] $ text "Hackage"
    text ", including all function and type definitions. Here are some example queries:"
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map&apos;); return false;"
         ,A.href "hayoo.html?query=map&amp;start=0"
         ] $ text "map"
      text " searches for everything that contains a word starting with \"map\""
      text " (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;name:map&apos;); return false;"
         ,A.href "hayoo.html?query=name%3Amap&amp;start=0"
         ] $ text "name:map"
      text " searches for everything where the function name starts with \"map\" (case insensitive)."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map OR fold&apos;); return false;"
         ,A.href "hayoo.html?query=map%20OR%20fold&amp;start=0"
         ] $ text "map OR fold"
      text " searches for everything that contains a word starting with \"map\" or \"fold\""
      text " (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map package:containers&apos;); return false;"
         ,A.href "hayoo.html?query=map%20package%3Acontainers&amp;start=0"
         ] $ text "map package:containers"
      text " searches for everything from package \"containers\" that contains a word"
      text " starting with \"map\" (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map hierarchy:Lazy&apos;); return false;"
         ,A.href "hayoo.html?query=map%20hierarchy%3ALazy&amp;start=0"
         ] $ text "map hierarchy:Lazy"
      text " searches for everything where \"Lazy\" appears somewhere in the full qualified module"
      text " name and that contains a word starting with \"map\" (case insensitive)"
      text " in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;(map OR fold) module:Data.Map&apos;); return false;"
         ,A.href "hayoo.html?query=(map%20OR%20fold)%20module%3AData.Map&amp;start=0"
         ] $ text "(map OR fold) module:Data.Map"
      text " searches for everything from module \"Data.Map\" that contains a word"
      text " starting with \"map\" or \"fold\" (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;name:attr module:Text.XML&apos;); return false;"
         ,A.href "hayoo.html?query=name%3Aattr%20module%3AText.XML&amp;start=0"
         ] $ text "name:attr module:Text.XML"
      text " searches for everything from the whole module hierarchy \"Text.XML\""
      text " where the function name starts with \"attr\" (case insensitive)."

help :: XHtml FlowContent
help = div' [A.id_ "result"] $ do
  div' [A.id_ "status"] $ text "Enter some search terms above to start a search."

  div' [A.id_ "helptext", A.class_ "text"] $ do
    h2 $ text "Basic Usage"
    p $ do
      text "By default, Hayoo! searches for function names, module names, signatures and function descriptions."
      text " With every letter typed, Hayoo! will show the results it thinks are best matching the query as well"
      text " as some suggestions on how the words from the query could be completed."
      text " Clicking one of these suggestions will replace the according word in the query."
    p $ do
      text "Hayoo! displays results as a list of functions, including full qualified module name"
      text " and the function signature. Clicking the function name will lead directly to the"
      text " corresponding documentation while clicking the module name will lead to the documentation"
      text " of the module. Additionally, Hayoo! shows the function description (if available) and"
      text " provides a link leading directly to the source of the function (if available)."
      text " The description of the function can be expanded by clicking on the small '+' sign."
    p $ do
      text "Along with the results, Hayoo! shows two lists on the right, containing the top"
      text " fifteen root-modules and packages. These are aggregated from the actual results."
      text " Clicking on each of these will further restrict the current query to the"
      text " respective module hierarchy or package. On the left side, package search"
      text " results are shown if the query matches the package information."

    h2 $ text "Advanced Queries"
    p $ do
      text "If words are seperated by whitespace, Hayoo! will search for results containing both words."
      text " Instead of using whitespace, the explicit "
      span' [A.class_ "query"] $ text "AND"
      text " operator can be used. Hayoo! also supports "
      span' [A.class_ "query"] $ text "OR"
      text " and "
      span' [A.class_ "query"] $ text "NOT"
      text " operators, although the "
      span' [A.class_ "query"] $ text "NOT"
      text " operator may only be used together with "
      span' [A.class_ "query"] $ text "AND"
      text ", e.g. "
      span' [A.class_ "query"] $ text "map NOT fold"
      text " or "
      span' [A.class_ "query"] $ text "map AND NOT fold"
      text ". Operator precedence can be influenced using round parentheses."
      text " Phrases can be searched using double quotes, e.g. "
      span' [A.class_ "query"] $ text "\"this is a phrase\""
      text "."
 
    p $ do
      text "It is possible to restrict a search to certain packages or modules."
      text " The most simple way would be to just include the package name in the search, e.g. "
      span' [A.class_ "query"] $ text "map base"
      text " will prefer hits from the base package. But the restriction can also be more explicit, like "
      span' [A.class_ "query"] $ text "map package:base"
      text " or like "
      span' [A.class_ "query"] $ text "map module:data.list"
      text ". It is also possible to specify several different modules or packages, like this: "
      span' [A.class_ "query"] $ text "fold module:(data.list OR data.map)"
      text ". This will return all hits for fold in the module hierarchies below Data.List and Data.Map."
    p $ do
      text "Hayoo! always performs fuzzy queries. This means, it tries to find something"
      text " even if the query contains spelling errors. For example, Hayoo! will still"
      text " find \"fold\" if \"fodl\" is being searched. If Hayoo! detects \">\" in the"
      text " query string, it will only search for signatures. A signature query may consist"
      text " of explicit type names as well as type variables. For example,"
      text " searching for \"a > b\" will find signatures like \"Int > Bool\"."

    h2 $ text "Scope"
    p $ do
      text "Currently, Hayoo! searches all packages available on "
      a' [A.href "http://hackage.haskell.org"] $ text "Hackage"
      text ". Additionally, any Haskell documentation generated by Haddock can be"
      text " included in Hayoo!. Just send a message including"
      text " an URI where the documentation can be found to "
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
      text "."

about :: XHtml FlowContent
about = div' [A.id_ "result"] $ do
  div' [A.id_ "status"] $ text "Enter some search terms above to start a search."

  div' [A.id_ "abouttext", A.class_ "text"] $ do
    h2 $ text "About Hayoo!"
    p $ do
      text "Hayoo! is a search engine specialized on " 
      a' [A.href "http://www.haskell.org"] $ text "Haskell"
      text " API documentation. The goal of Hayoo! is to provide an interactive,"
      text " easy-to-use search interface to the documenation of various Haskell"
      text " packages and libraries. Although the Hayoo! data is regularly updated,"
      text " we might miss a package or library. If you think there is some"
      text " documentation for Haskell modules available on the Internet which"
      text " should be added to Hayoo!, just drop us a note at "
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
      text " and tell us the location where we can find the documentation."

    h2 $ text "Background"
    p $ do
      text "Hayoo! is an example application of the "
      a' [A.href "http://holumbus.fh-wedel.de"] $ text "Holumbus"
      text " framework and was heavily inspired by "
      a' [A.href "http://www.haskell.org/hoogle"] $ text "Hoogle"
      text ". The Holumbus library provides the search and indexing backend for Hayoo!."
      text " Holumbus and Hayoo! have been developed by"
      text " Sebastian M. Gauck and Timo B. Kranz (formerly H\252bel) at "
      a' [A.href "http://www.fh-wedel.de"] $ text "FH Wedel University of Applied Sciences"
      text ". The Holumbus framework provides the basic building blocks"
      text " for creating highly customizable search engines. To demonstrate the"
      text " flexibility of the framework by a very special use case, the Hayoo!"
      text " Haskell API search was implemented using Holumbus."
    p $ do
      text "Currently, Hayoo! is still in beta stage. This means, it can become"
      text " unavailable unexpectedly, as we do some maintenance or add new features."
      text " Therefore you should not yet rely on Hayoo! as primary search engine for Haskell documentation."
    p $ do
      text "Hardware infrastructure for daily index updates is generously sponsored by "
      a' [A.href "http://www.fortytools.com"] $ text "fortytools gmbh"
      text ", your friendly Haskell web development company."

    h2 $ text "Technical Information"
    p $ do
      text "Hayoo! is written entirely in Haskell and consists of two main parts:"
      text " The indexer, which regularly checks Hackage for package updates and"
      text " builds the search index and the web frontend. The web frontend formerly worked with apache and FastCGI"
      text " today it's implemented with the use of the Snap system."

    h2 $ text "Feedback"
    p $ do
      text "We would like to know what you think about Hayoo!, therefore you can reach us at "
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
      text " and tell us about bugs, suggestions or anything else related to Hayoo!."
    div' [A.id_ "sponsors"] $ do
      div' [A.id_ "hol"] $
        a' [A.href "http://holumbus.fh-wedel.de"] $
           img' "" "" [A.src "hayoo/hol.png", A.alt "Holumbus logo", A.class_ "logo"] -- Change here when img bug in xhtml-combinators is fixed
      div' [A.id_ "ft"] $
        a' [A.href "http://www.fortytools.com"] $
           img' "" "" [A.src "hayoo/ft.png", A.alt "fortytools logo", A.class_ "logo"]
      div' [A.id_ "fhw"] $
        a' [A.href "http://www.fh-wedel.de"] $
           img' "" "" [A.src "hayoo/fhw.gif", A.alt "FH-Wedel logo", A.class_ "logo"]
      
api :: XHtml FlowContent
api = div' [A.id_ "result"] $ do
  div' [A.id_ "status"] $ text "Enter some search terms above to start a search."

  div' [A.id_ "helptext", A.class_ "text"] $ do
    h2 $ text "Hayoo! API"
    p $ do
      text "Hayoo! provides a JSON-based webservice API, which can be used"
      text " to retrieve search results in a structured format. This allows"
      text " one to include Hayoo! search functionality in other applications."
      text " Arbitrary queries can be submitted the same way as they would be"
      text " entered them into the search box and results are returned encoded in JSON format."

    p $ do
      text "You may use this service for whatever you like and without any limitations,"
      text " although we would be very happy to know about any application that uses"
      text " the Hayoo! webservice API. Just drop us a line at"
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
      text "."      

    h2 $ text "Request URI"
    p $ text "Direct your search request to the following URI:"
    pre $ text "http://holumbus.fh-wedel.de/hayoo/hayoo.json?query=YOUR_QUERY"
    p $ do
      text "Provide your query as argument to the "
      code $ text "query"
      text " URI parameter. Please note that you need to ensure proper URI encoding"
      text " for the query argument. The syntax for the query is the same as if it"
      text " would be entered into the search box. A detailed explanation of the syntax can be found " 
      a' [A.href "help.html"] $ text "here"
      text "."

    h2 $ text "Response"
    p $ do
      text "The response to a search request will be encoded in "
      a' [A.href "http://www.json.org"] $ text "JSON"
      text " format and is structured as follows:"
    pre $ code $ sequence_ $ map text $
        [ "{\n"
        , "  \"message\":\"Found 12 results and 17 completions.\",\n"
        , "  \"hits\":12,\n"
        , "  \"functions\":[ {\n"
        , "    \"name\":\"map\",\n"
        , "    \"uri\":\"http://hackage.haskell.org/...\",\n"
        , "    \"module\":\"Data.Map\",\n"
        , "    \"signature\":\"(a->b)->[a]->[b]\",\n"
        , "    \"package\":\"containers\"\n"
        , "  }, ... ],\n"
        , "  \"completions\":[ {\n"
        , "    \"word\":\"MapM\",\n"
        , "    \"count\":11\n"
        , "  }, ... ],\n"
        , "  \"modules\":[ {\n"
        , "    \"name\":\"Data\",\n"
        , "    \"count\":19\n"
        , "  }  }, ... ],\n"
        , "  \"packages\":[ {\n"
        , "    \"name\":\"containers\",\n"
        , "    \"count\":13\n"
        , "  }, ... ]\n"
        , "}"
        ]
    p $ do
      text "The "
      ct "message"
      text " field will contain a descriptive status message about the result or any errors encountered. The "
      ct "hits"
      text " field will contain the total number of functions found. In the "
      ct "functions"
      text " field, an array containing all functions found will be returned."
      text " For every function, a JSON object is included in the array."
    p $ do
      text "Each of these objects contains the function name, the URI pointing"
      text " to the Haddock documentation, the module, the signature and the package name in the "
      ct "name"
      text ", "
      ct "uri"
      text ", "
      ct "module"
      text ", "
      ct "signature"
      text" and "
      ct "package"
      text " fields, respectively."
    p $ do
      text "The "
      ct "completions"
      text " contains all word completions (suggestions) resulting from the query For every completion,"
      text " a JSON object is included in the array, containing the suggested word and"
      text " the total number of occurrences of this word in the search result in the "
      ct "word"
      text " and "
      ct "count"
      text " fields."
    p $ do
      text "The "
      ct "modules"
      text " and "
      ct "packages"
      text " fields contain arrays with JSON objects denoting the occurrences of root"
      text " modules and packages in the search result. For each element, the module/package name is included in the "
      ct "name"
      text " field and the number of occurrences in the "
      ct "count"
      text " field."

ct :: (Functor t, Monad t, Inline c) => Text -> XHtmlT t c
ct = code . text

