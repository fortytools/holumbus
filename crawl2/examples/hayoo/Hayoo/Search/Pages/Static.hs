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

{-# LANGUAGE OverloadedStrings #-}

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
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map&apos;); return false;", A.href "hayoo.html?query=map&amp;start=0"] $ text "map"
      text " searches for everything that contains a word starting with \"map\" (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;name:map&apos;); return false;", A.href "hayoo.html?query=name%3Amap&amp;start=0"] $ text "name:map"
      text " searches for everything where the function name starts with \"map\" (case insensitive)."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map OR fold&apos;); return false;", A.href "hayoo.html?query=map%20OR%20fold&amp;start=0"] $ text "map OR fold"
      text " searches for everything that contains a word starting with \"map\" or \"fold\" (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map package:containers&apos;); return false;", A.href "hayoo.html?query=map%20package%3Acontainers&amp;start=0"] $ text "map package:containers"
      text " searches for everything from package \"containers\" that contains a word starting with \"map\" (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;map hierarchy:Lazy&apos;); return false;", A.href "hayoo.html?query=map%20hierarchy%3ALazy&amp;start=0"] $ text "map hierarchy:Lazy"
      text " searches for everything where \"Lazy\" appears somewhere in the full qualified module name \
        \and that contains a word starting with \"map\" (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;(map OR fold) module:Data.Map&apos;); return false;", A.href "hayoo.html?query=(map%20OR%20fold)%20module%3AData.Map&amp;start=0"] $ text "(map OR fold) module:Data.Map"
      text " searches for everything from module \"Data.Map\" that contains a word starting with \"map\" or \"fold\" (case insensitive) in the function name, module name or description."
    div' [A.class_ "example"] $ p $ do
      a' [A.attr "onclick" "replaceInQuery(&apos;&apos;,&apos;name:attr module:Text.XML&apos;); return false;", A.href "hayoo.html?query=name%3Aattr%20module%3AText.XML&amp;start=0"] $ text "name:attr module:Text.XML"
      text " searches for everything from the whole module hierarchy \"Text.XML\" where the function name starts with \"attr\" (case insensitive)."

help :: XHtml FlowContent
help = div' [A.id_ "result"] $ do
  div' [A.id_ "status"] $ text "Enter some search terms above to start a search."

  div' [A.id_ "helptext", A.class_ "text"] $ do
    h2 $ text "Basic Usage"
    p $ do
      text "By default, Hayoo! searches for function names, module names, signatures and function \
        \descriptions. With every letter typed, Hayoo! will show the results it thinks are best matching \
        \the query as well as some suggestions on how the words from the query could be completed. \
        \Clicking one of these suggestions will replace the according word in the query."
    p $ do
      text "Hayoo! displays results as a list of functions, including full qualified module name and the \
        \function signature. Clicking the function name will lead directly to the corresponding documentation \
        \while clicking the module name will lead to the documentation of the module. Additionally, Hayoo! \
        \shows the function description (if available) and provides a link leading directly to the source \
        \of the function (if available). The description of the function can be expanded by clicking on \
        \the small '+' sign."
    p $ do
      text "Along with the results, Hayoo! shows two lists on the right, containing the top fifteen \
        \root-modules and packages. These are aggregated from the actual results. Clicking on each of \
        \these will further restrict the current query to the respective module hierarchy or package. \
        \On the left side, package search results are shown if the query matches the package information."

    h2 $ text "Advanced Queries"
    p $ do
      text "If words are seperated by whitespace, Hayoo! will search for results containing both words. \
        \Instead of using whitespace, the explicit "
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
      text ". Operator precedence can be influenced using round parentheses. Phrases can be searched \
        \using double quotes, e.g. "
      span' [A.class_ "query"] $ text "\"this is a phrase\""
      text "."
 
    p $ do
      text "It is possible to restrict a search to certain packages or modules. The most simple way would \
        \be to just include the package name in the search, e.g. "
      span' [A.class_ "query"] $ text "map base"
      text " will prefer hits from the base package. But the restriction can also be more explicit, like "
      span' [A.class_ "query"] $ text "map package:base"
      text " or like "
      span' [A.class_ "query"] $ text "map module:data.list"
      text ". It is also possible to specify several different modules or packages, like this: "
      span' [A.class_ "query"] $ text "fold module:(data.list OR data.map)"
      text ". This will return all hits for fold in the module hierarchies below Data.List and Data.Map."
    p $ do
      text "Hayoo! always performs fuzzy queries. This means, it tries to find something even if the \
        \query contains spelling errors. For example, Hayoo! will still find \"fold\" if \"fodl\" is \
        \being searched. If Hayoo! detects \">\" in the query string, it will only search for signatures. \
        \A signature query may consist of explicit type names as well as type variables. For example, \
        \searching for \"a > b\" will find signatures like \"Int > Bool\"."

    h2 $ text "Scope"
    p $ do
      text "Currently, Hayoo! searches all packages available on "
      a' [A.href "http://hackage.haskell.org"] $ text "Hackage"
      text ". Additionally, any Haskell documentation generated by Haddock can be included in Hayoo!. \
        \Just send a message including an URI where the documentation can be found to "
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
      text " API documentation. The goal of Hayoo! is to provide an interactive, easy-to-use search interface to \
        \the documenation of various Haskell packages and libraries. Although the Hayoo! data is regularly updated, \
        \we might miss a package or library. If you think there is some documentation for Haskell modules available \
        \on the Internet which should be added to Hayoo!, just drop us a note at "
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
      text " and tell us the location where we can find the documentation."

    h2 $ text "Background"
    p $ do
      text "Hayoo! is an example application of the "
      a' [A.href "http://holumbus.fh-wedel.de"] $ text "Holumbus"
      text " framework and was heavily inspired by "
      a' [A.href "http://www.haskell.org/hoogle"] $ text "Hoogle"
      text ". The Holumbus library provides the search and indexing backend for Hayoo!. Holumbus and Hayoo! \
        \have been developed by Sebastian M. Gauck and Timo B. Hübel at "
      a' [A.href "http://www.fh-wedel.de"] $ text "FH Wedel University of Applied Sciences"
      text ". The Holumbus framework provides the basic building blocks for creating highly customizable search \
        \engines. To demonstrate the flexibility of the framework by a very special use case, the Hayoo! Haskell \
        \API search was implemented using Holumbus."
    p $ do
      text "Currently, Hayoo! is still in beta stage. This means, it can become unavailable unexpectedly, as \
        \we do some maintenance or add new features. Therefore you should not yet rely on Hayoo! as primary \
        \ search engine for Haskell documentation."
 
    h2 $ text "Technical Information"
    p $ do
      text "Hayoo! is written entirely in Haskell and consists of two main parts: The indexer, which regularly \
        \checks Hackage for package updates and builds the search index and the web frontend, which relies on \
        \Apache, FastCGI and Hack for presenting search results to the user."

    h2 $ text "Feedback"
    p $ do
      text "We would like to know what you think about Hayoo!, therefore you can reach us at "
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
      text " and tell us about bugs, suggestions or anything else related to Hayoo!."
 
    div' [A.id_ "hol"] $ do
      a' [A.href "http://holumbus.fh-wedel.de"] $ img' "" "" [A.src "hayoo/hol.png", A.alt "Holumbus logo", A.class_ "logo"] -- Change here when img bug in xhtml-combinators is fixed
    div' [A.id_ "fhw"] $ do
      a' [A.href "http://www.fh-wedel.de"] $ img' "" "" [A.src "hayoo/fhw.gif", A.alt "FH-Wedel logo", A.class_ "logo"]
      
api :: XHtml FlowContent
api = div' [A.id_ "result"] $ do
  div' [A.id_ "status"] $ text "Enter some search terms above to start a search."

  div' [A.id_ "helptext", A.class_ "text"] $ do
    h2 $ text "Hayoo! API"
    p $ do
      text "Hayoo! provides a JSON-based webservice API, which can be used to retrieve search results in a structured \
           \format. This allows one to include Hayoo! search functionality in other applications. Arbitrary queries \
           \can be submitted the same way as they would be entered them into the search box and results are returned \
           \encoded in JSON format."

    p $ do
      text "You may use this service for whatever you like and without any limitations, although we would be \
           \very happy to know about any application that uses the Hayoo! webservice API. Just drop us a line at"
      a' [A.href "mailto:hayoo@holumbus.org"] $ text "hayoo@holumbus.org"
      text "."      

    h2 $ text "Request URI"
    p $ text "Direct your search request to the following URI:"
    pre $ text "http://holumbus.fh-wedel.de/hayoo/hayoo.json?query=YOUR_QUERY"
    p $ do
      text "Provide your query as argument to the "
      code $ text "query"
      text " URI parameter. Please note that you need to ensure proper URI encoding for the query argument. The syntax \
           \for the query is the same as if it would be entered into the search box. A detailed explanation of the \
           \syntax can be found " 
      a' [A.href "help.html"] $ text "here"
      text "."

    h2 $ text "Response"
    p $ do
      text "The response to a search request will be encoded in "
      a' [A.href "http://www.json.org"] $ text "JSON"
      text " format and is structured as follows:"
    pre $ do
      code $ text "{\n\
                 \  \"message\":\"Found 12 results and 17 completions.\",\n\
                 \  \"hits\":12,\n\
                 \  \"functions\":[ {\n\
                 \    \"name\":\"map\",\n\
                 \    \"uri\":\"http://hackage.haskell.org/...\",\n\
                 \    \"module\":\"Data.Map\",\n\
                 \    \"signature\":\"(a->b)->[a]->[b]\",\n\
                 \    \"package\":\"containers\"\n\
                 \  }, ... ],\n\
                 \  \"completions\":[ {\n\
                 \    \"word\":\"MapM\",\n\
                 \    \"count\":11\n\
                 \  }, ... ],\n\
                 \  \"modules\":[ {\n\
                 \    \"name\":\"Data\",\n\
                 \    \"count\":19\n\
                 \  }  }, ... ],\n\
                 \  \"packages\":[ {\n\
                 \    \"name\":\"containers\",\n\
                 \    \"count\":13\n\
                 \  }, ... ]\n\
                 \}"
    p $ do
      (text "The ") >> (ct "message") >> (text " field will contain a descriptive status message about the result \
        \or any errors encountered. The ") >> (ct "hits") >> (text " field will contain the total number of \
        \functions found. In the ") >> (ct "functions") >> (text " field, an array containing all functions found \
        \will be returned. For every function, a JSON object is included in the array.")
    p $ do
      (text "Each of these objects contains the function name, the URI pointing to the Haddock documentation, the module, \
        \the signature and the package name in the ") >> (ct "name") >> (text ", ") >> (ct "uri") >> (text ", ")
        >> (ct "module") >> (text ", ") >> (ct "signature") >> (text" and ") >> (ct "package") >> (text " fields, respectively.")
    p $ do
      (text "The ") >> (ct "completions") >> (text " contains all word completions (suggestions) resulting from the query \
        \For every completion, a JSON object is included in the array, containing the suggested word and the total number \
        \of occurrences of this word in the search result in the ") >> (ct "word") >> (text " and ") >> (ct "count")
        >> (text " fields.")
    p $ do
      (text "The ") >> (ct "modules") >> (text " and ") >> (ct "packages") >> (text " fields contain arrays with JSON objects \
        \denoting the occurrences of root modules and packages in the search result. For each element, the module/package \
        \name is included in the ") >> (ct "name") >> (text " field and the number of occurrences in the ") >> (ct "count")
        >> (text " field.")

ct :: (Functor t, Monad t, Inline c) => Text -> XHtmlT t c
ct = code . text

