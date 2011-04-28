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

import Control.Concurrent               -- For the global MVar

import Data.ByteString.Lazy.Char8       ( fromChunks )

import qualified Data.List              as L
import qualified Data.Text.Encoding     as T

import Hayoo.IndexTypes
import Hayoo.Search.EvalSearch
import Hayoo.Search.Pages.Template
import Hayoo.Search.Pages.Static

import Holumbus.Index.Common

import Hack
import Hack.Contrib.Middleware.File
import Hack.Contrib.Middleware.URLMap
import Hack.Contrib.Request             ( params, path, host )
import Hack.Contrib.Utils               ( empty_app )

import System.IO                        ( stdout )
import System.Time

import System.Log.Logger
import System.Log.Handler.Simple

import qualified
       Text.XHtmlCombinators            as X

-- ------------------------------------------------------------

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
                    , body = fromChunks [T.encodeUtf8 $ X.render c]
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

-- ------------------------------------------------------------
