-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Search.JSON
  Copyright  : Copyright (C) 2010 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Rendering a Hayoo! search result into a JSON string.

-}

-- ----------------------------------------------------------------------------

module Hayoo.Search.JSON
    ( renderJson
    , renderEmptyJson
    )
where

import Data.Function

import qualified Data.Map as M
import qualified Data.List as L

import Text.JSON

import Holumbus.Index.Common
import Holumbus.Query.Result

import Hayoo.IndexTypes
import Hayoo.Search.Common

-- ----------------------------------------------------------------------------

renderEmptyJson         :: String
renderEmptyJson         = encodeStrict $
                          jo 
                          [ ("message", js "Please provide a query.")
                          , ("hits", showJSON (0 :: Int))
                          , ("functions", JSArray []) 
                          , ("completions", JSArray []) 
                          , ("modules", JSArray []) 
                          , ("packages", JSArray []) 
                          ]

renderJson              :: StatusResult -> String
renderJson (msg, res, _, mods, pkgs)
                        = encodeStrict $
                          jo
                          [ ("message",      js msg)
                          , ("hits",         showJSON      $ sizeDocHits res)
                          , ("functions" ,   buildDocHits  $ docHits res)
                          , ("completions" , buildWordHits $ wordHits res)
                          , ("modules",      buildTopList    mods)
                          , ("packages",     buildTopList    pkgs)
                          ]

buildDocHits            :: DocHits FunctionInfo -> JSValue
buildDocHits dh         = JSArray $ map buildDoc $ reverse $ take 100 $ L.sortBy (compare `on` (docScore . fst. snd)) $ toListDocIdMap dh

buildDoc                :: (DocId, (DocInfo FunctionInfo, DocContextHits)) -> JSValue
buildDoc (_, (DocInfo (Document t u (Just fi)) _, _))
                        = jo
                          [ ("name",        js t)
                          , ("uri",         js u)
                          , ("module",      js $ moduleName fi)
                          , ("signature",   js $ signature fi)
                          , ("package",     js $ package fi)
                          , ("description", js $ fctDescr fi)
                          ]
buildDoc _              = error "Expected custom function info"

buildWordHits           :: WordHits -> JSValue
buildWordHits wh        = JSArray $ map buildWord (take 500 $ L.sortBy (compare `on` (wordScore . fst . snd)) $ M.toList wh)

buildWord               :: (Word, (WordInfo, WordContextHits)) -> JSValue
buildWord (w, (WordInfo _ s, _))
                        = jo
                          [ ("word", js w)
                          , ("count", showJSON s)
                          ]

buildTopList            :: [(String, Int)] -> JSValue
buildTopList            = showJSON . map buildTopElem
  where
  buildTopElem (n, c)   = jo
                          [ ("name", js n)
                          , ("count", showJSON c)
                          ]

js                      :: String -> JSValue
js                      = JSString . toJSString

jo                      :: [(String, JSValue)] -> JSValue
jo                      = JSObject . toJSObject

