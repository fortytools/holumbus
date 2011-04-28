{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

------------------------------------------------------------------------------

module Hayoo.Snap.Site
  ( site
  ) where

import           Control.Applicative

import qualified Data.ByteString.Char8          as C
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T

import           Hayoo.Search.EvalSearch        ( Core
                                                , template
                                                , getValDef
                                                , readDef

                                                , parseQuery
                                                , genResult
                                                , emptyRes

                                                , renderJson
                                                , renderEmptyJson

                                                , examples
                                                , filterStatusResult
                                                )
import           Hayoo.Search.HTML              ( RenderState(..)
                                                , result
                                                )

import           Hayoo.Snap.Extension.HayooState
import           Hayoo.Snap.Application

import           System.FilePath                ( (</>) )

import           Snap.Util.FileServe
import           Snap.Types

import qualified Text.XHtmlCombinators          as X


------------------------------------------------------------------------------
-- | The main entry point handler.

site :: Application ()
site = route [ ("/",             ifTop hayooHtml)       -- map to /hayoo.html
             , ("/hayoo.html",   hayooHtml)
             , ("/hayoo.json",   hayooJson)
             , ("/hayoo/:stuff", serveHayooStatic)
             ]
       <|> serveDirectory "hayoo"

------------------------------------------------------------------------------
-- | Renders the echo page.

serveHayooStatic :: Application ()
serveHayooStatic = do
    relPath <- decodedParam "stuff"
    serveFile $ "hayoo" </> C.unpack relPath
  where
    decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
-- | Render JSON page

hayooJson :: Application ()
hayooJson = do
    pars <- getParams
    core <- hayooCore
    putResponse myResponse
    writeText (T.pack $ evalJsonQuery (toStringMap pars) core)
  where
  myResponse = setContentType "application/json; charset=utf-8"
               . setResponseCode 200
               $ emptyResponse
  toStringMap   = map (uncurry tos) . M.toList
      where
      tos k a   = (            T.unpack . T.decodeUtf8  $ k
                  , concatMap (T.unpack . T.decodeUtf8) $ a
                  )

evalJsonQuery :: [(String, String)] -> Core -> String
evalJsonQuery p idct
    | null request      = renderEmptyJson
    | otherwise         = renderResult
    where
    request =               getValDef p "query"  ""

    {- not used for json output
    start   = readDef 0    (getValDef p "start"  "")
    static  = readDef True (getValDef p "static" "")
    tmpl    = template idct
    -}

    renderResult        = renderJson
                          . either emptyRes (genResult idct)
                          . parseQuery
                          $ request

------------------------------------------------------------------------------
-- | Render HTML page

hayooHtml :: Application ()
hayooHtml = do
    pars <- getParams
    core <- hayooCore
    putResponse myResponse
    writeText $ evalHtmlQuery (toStringMap pars) core
  where
  myResponse = setContentType "text/html; charset=utf-8"
               . setResponseCode 200
               $ emptyResponse
  toStringMap   = map (uncurry tos) . M.toList
      where
      tos k a   = (            T.unpack . T.decodeUtf8  $ k
                  , concatMap (T.unpack . T.decodeUtf8) $ a
                  )

evalHtmlQuery :: [(String, String)] -> Core -> T.Text
evalHtmlQuery p idct
    | null request      = renderEmptyHtml
    | otherwise         = renderResult
    where
    request =               getValDef p "query"  ""
    start   = readDef 0    (getValDef p "start"  "")
    static  = readDef True (getValDef p "static" "")
    tmpl    = template idct

    renderEmptyHtml     = X.render $ tmpl examples

    renderResult        = applyTemplate (RenderState request start static)
                          . filterStatusResult request
                          . either emptyRes (genResult idct)
                          . parseQuery
                          $ request
        where
        applyTemplate rs sr
            | rsStatic rs       = X.render $ tmpl rr
            | otherwise         = X.render $      rr
            where
            rr                  = result rs sr

------------------------------------------------------------------------------
