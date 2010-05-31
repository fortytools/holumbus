  -- ----------------------------------------------------------------------------

{- |
  Module     : Main
  Copyright  : Copyright (C) 2010 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Main program to use as standalone webserver for serving Hayoo!

-}

-- ----------------------------------------------------------------------------

module Main where

import qualified Data.List as L

import Hayoo.IndexTypes			-- the crawl2 changes
import Hayoo.Search.Application

import Holumbus.Index.Common

import Text.XML.HXT.Arrow 		hiding (app)

import Hack.Handler.SimpleServer

import Hack.Contrib.Utils

import Hack.Contrib.Middleware.File
import Hack.Contrib.Middleware.URLMap

import System.IO

import System.Log.Logger
import System.Log.Handler.Simple

import Control.Concurrent  		-- For the global MVar

-- ----------------------------------------------------------------------------

type Template = XmlTree

-- | Number of threads to use for serving requests

numThreads 	:: Int
numThreads 	= 4

-- | Just an alias with explicit type.

loadIndex 	:: FilePath -> IO CompactInverted
loadIndex 	= loadFromFile

-- | Just an alias with explicit type.

loadDocuments 	:: FilePath -> IO (SmallDocuments FunctionInfo)
loadDocuments 	= loadFromFile

-- | Just an alias with explicit type.

loadPkgDocs 	:: FilePath -> IO (SmallDocuments PackageInfo)
loadPkgDocs 	= loadFromFile

-- | Load the template.

loadTemplate 	:: FilePath -> IO XmlTree
loadTemplate f 	= do
                  tpl <- runX $ readDocument [ (a_parse_html,v_1)
                                             , (a_indent,v_1)
                                             , (a_trace,v_1)
                                             ] f
                  if L.null tpl
                    then error "Unable to read template"
                    else return $ head tpl

ixBase, wwwBase	:: String -> String
ixBase		= ("./" ++)
wwwBase		= ("../../../searchengine/examples/hayoo/wwwpages/" ++)

-- | The main application, fire up the FastCGI handler here!
main 		:: IO ()
main 		= do
                  fdl  <- fileHandler "hayoo.log" INFO
                  sdl  <- streamHandler stdout INFO

                  updateGlobalLogger rootLoggerName (setHandlers [fdl, sdl])
                  updateGlobalLogger rootLoggerName (setLevel INFO)

                  idx  <- loadIndex     $ ixBase  "ix.bin.idx"
                  doc  <- loadDocuments $ ixBase  "ix.bin.doc"
                  pidx <- loadIndex     $ ixBase  "pkg.bin.idx"
                  pdoc <- loadPkgDocs   $ ixBase  "pkg.bin.doc"
                  tpl  <- loadTemplate  $ wwwBase "hayoo.html"

                  midct <- newMVar $
                           Core
                           { index 	= idx
                           , documents 	= doc
                           , pkgIndex	= pidx
                           , pkgDocs	= pdoc
                           , template 	= tpl
                           }

                  app <- return $
                         url_map [ ("/hayoo.html", hayooApplication midct)
                                 , ("/hayoo.json", hayooApplication midct)
                                 ] (file Nothing empty_app)
                  run 4242 $ app

-- ----------------------------------------------------------------------------
