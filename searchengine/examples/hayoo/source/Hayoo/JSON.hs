-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.JSON
  Copyright  : Copyright (C) 2010 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Rendering a Hayoo! search result into a JSON string.

-}

-- ----------------------------------------------------------------------------

module Hayoo.JSON (renderJson, renderEmptyJson) where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.UTF8 as B

import Text.JSON

import Holumbus.Index.Common
import Holumbus.Query.Result

import Hayoo.Common

renderEmptyJson :: String
renderEmptyJson = encodeStrict $ jo 
  [ ("message", js "Please provide a query.")
  , ("hits", showJSON (0 :: Int))
  , ("functions", JSArray []) 
  , ("completions", JSArray []) 
  , ("modules", JSArray []) 
  , ("packages", JSArray []) 
  ]

renderJson :: HolCache c => StatusResult -> c -> String
renderJson (msg, res, mods, pkgs) c = encodeStrict value
  where
  value = jo
    [ ("message", js msg)
    , ("hits", showJSON $ sizeDocHits res)
    , ("functions" , buildDocHits c $ docHits res)
    , ("completions" , buildWordHits $ wordHits res)
    , ("modules", buildTopList mods)
    , ("packages", buildTopList pkgs)
    ]

buildDocHits :: HolCache c => c -> DocHits FunctionInfo -> JSValue
buildDocHits c dh = JSArray $ map (buildDoc c) (IM.toList dh)

buildDoc :: HolCache c => c -> (Int, (DocInfo FunctionInfo, DocContextHits)) -> JSValue
buildDoc _ (_, (DocInfo (Document t u (Just (FunctionInfo m s p _))) _, _)) = jo
  [ ("name", js t)
  , ("uri", js u)
  , ("module", js $ B.toString $ m)
  , ("signature", js $ B.toString $ s)
  , ("package", js $ B.toString $ p)
  ]
buildDoc _ _ = error "Expected custom function info"

buildWordHits :: WordHits -> JSValue
buildWordHits wh = JSArray $ map buildWord (M.toList wh)

buildWord :: (Word, (WordInfo, WordContextHits)) -> JSValue
buildWord (w, (WordInfo _ s, _)) = jo
  [ ("word", js w)
  , ("count", showJSON s)
  ]

buildTopList :: [(String, Int)] -> JSValue
buildTopList = showJSON . map buildTopElem
  where
  buildTopElem (n, c) = jo [("name", js n), ("count", showJSON c) ]

js :: String -> JSValue
js = JSString . toJSString

jo :: [(String, JSValue)] -> JSValue
jo = JSObject . toJSObject

