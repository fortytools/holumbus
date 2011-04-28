{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Hayoo.Snap.Site
  ( site
  ) where

import           Control.Applicative
-- import           Control.Monad.IO.Class

import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Text.Encoding             as T
import qualified Data.Text                      as T

import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Types

import           Text.Templating.Heist

import           Hayoo.Search.EvalSearch        ( Core
                                                -- , template
                                                , getValDef
                                                -- , readDef
                                                , renderJson
                                                , renderEmptyJson
                                                , parseQuery
                                                , genResult
                                                , emptyResult
                                                )

import           Hayoo.Snap.Extension.Timer
import           Hayoo.Snap.Extension.HayooState
import           Hayoo.Snap.Application


------------------------------------------------------------------------------
-- | The main entry point handler.

site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             -- , ("/hayoo.html",  hayooHtml)
             , ("/hayoo.json",  hayooJson)
             ]
       <|> serveDirectory "hayoo"
       <|> serveDirectory "resources/static"

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


------------------------------------------------------------------------------
-- | Renders the echo page.

echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    pars    <- decodedQuery
    heistLocal (bindString "message" (T.decodeUtf8 message `T.append` T.pack pars)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p
    decodedQuery   = getParams >>= return . show

------------------------------------------------------------------------------
-- | Render test page

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
        where
        emptyRes msg    = ( tail . dropWhile ((/=) ':') $ msg
                          , emptyResult
                          , emptyResult
                          , []
                          , []
                          )

------------------------------------------------------------------------------
