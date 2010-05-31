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

import Text.XML.HXT.Arrow
import Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch	( sed )

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
                                             , (a_trace,v_0)
                                             ] f
                  if L.null tpl
                    then error "Unable to read template"
                    else return $ head tpl

contentEdit     :: (String -> String) -> FilePath -> IO ()
contentEdit ef f
    = do
      h <- openFile f ReadMode
      c <- hGetContents h
      b <- openFile (f ++ "~") WriteMode        -- make backup file
      hPutStr b c
      hClose b
      hClose h
      h' <- openFile f WriteMode
      hPutStr h' (ef c)
      hClose h'

editNoOfFctPkg		:: Int -> Int -> FilePath -> IO ()
editNoOfFctPkg n p	= contentEdit (insFct . insPkg)
    where
    insPkg		= sed (const $ "currently " ++ show p ++ " packages") "currently( )+[0-9]+( )+packages"
    insFct		= sed (const $ fmt n ++ " function and type") "[0-9]+[.][0-9]+( )+function and type"
    fmt			= show >>> reverse >>> splitAt 3 >>> (\ (x, y) -> x ++ "." ++ y) >>> reverse

ixBase, wwwBase	:: String -> String
ixBase		= ("./" ++)
wwwBase		= ("./" ++)	-- ("../../../searchengine/examples/hayoo/wwwpages/" ++)

-- | The main application, fire up the FastCGI handler here!
main 		:: IO ()
main 		= do
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

                  tpl  <- loadTemplate  templ
                  infoM "Hayoo.Main" ("Template loaded from file "      ++ show templ)

                  editNoOfFctPkg (sizeDocs doc) (sizeDocs pdoc) "hayoo.html"
                  infoM "Hayoo.Main" ("Start page \"hayoo.html\" updated with # of functions")


                  midct <- newMVar $
                           Core
                           { index 	= idx
                           , documents 	= doc
                           , pkgIndex	= pidx
                           , pkgDocs	= pdoc
                           , template 	= tpl
                           }

                  apl <- return $
                         url_map [ ("/hayoo.html", hayooApplication midct)
                                 , ("/hayoo.json", hayooApplication midct)
                                 ] (file Nothing empty_app)
                  run 4242 $ apl
  where
  hayooIndex	= ixBase  "ix.bin.idx"
  hayooDocs     = ixBase  "ix.bin.doc"
  hackageIndex	= ixBase  "pkg.bin.idx"
  hackageDocs   = ixBase  "pkg.bin.doc"
  templ 	= wwwBase "hayoo.html"

-- ----------------------------------------------------------------------------
