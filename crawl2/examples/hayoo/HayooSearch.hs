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

import Text.XML.HXT.Arrow hiding (app)

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

import Hayoo.IndexTypes

import Hack.Handler.SimpleServer

import Hack.Contrib.Utils

import Hack.Contrib.Middleware.File
import Hack.Contrib.Middleware.URLMap

import System.IO

import System.Log.Logger
import System.Log.Handler.Simple

import Control.Concurrent  -- For the global MVar

import Hayoo.Common
import Hayoo.Search.Application

type Template = XmlTree

-- | Number of threads to use for serving requests
numThreads :: Int
numThreads = 4

-- | Just an alias with explicit type.
loadIndex :: FilePath -> IO CompactInverted
loadIndex = loadFromFile

-- | Just an alias with explicit type.
loadDocuments :: FilePath -> IO (SmallDocuments FunctionInfo)
loadDocuments = loadFromFile

-- | Just an alias with explicit type.
loadPkgDocs :: FilePath -> IO (SmallDocuments PackageInfo)
loadPkgDocs = loadFromFile

-- | Load the template.
loadTemplate :: FilePath -> IO XmlTree
loadTemplate f = do
  tpl <- runX $ readDocument [ (a_parse_html,v_1), (a_indent,v_1), (a_trace,v_1) ] f
  if L.null tpl then error "Unable to read template" else return $ head tpl

-- | The main application, fire up the FastCGI handler here!
main :: IO ()
main = do
  inv <- loadIndex "hayoo-index.bin"
  doc <- loadDocuments "hayoo-docs.bin"
  cac <- createCache "hayoo-cache.db"
  tpl <- loadTemplate "hayoo.html"
  fdl <- fileHandler "hayoo.log" INFO
  sdl <- streamHandler stdout INFO

  updateGlobalLogger rootLoggerName (setHandlers [fdl, sdl])
  updateGlobalLogger rootLoggerName (setLevel INFO)

  midct <- newMVar $ Core inv doc cac tpl
  app <- return $ url_map [ ("/hayoo.html", hayooApplication midct), ("/hayoo.json", hayooApplication midct) ] (file Nothing empty_app)
  run 4242 $ app


