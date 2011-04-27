{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Hayoo.Snap.Site
  ( site
  ) where

import           Control.Applicative

import           Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Data.Text          as T

import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Types

import           Text.Templating.Heist

import           Hayoo.Snap.Extension.Timer
import           Hayoo.Snap.Application


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
-- | The main entry point handler.

site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             ]
       <|> serveDirectory "hayoo"
       <|> serveDirectory "resources/static"

