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

module Hayoo.JSON (renderJson) where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.UTF8 as B

import Text.JSON

import Holumbus.Index.Common
import Holumbus.Query.Result

import Hayoo.Common

renderJson :: HolCache c => StatusResult -> c -> String
renderJson (msg, res, mods, pkgs) c = encode $ jo
  [ ("message", js msg)
  , ("hits", showJSON $ sizeDocHits res)
  , ("completions", showJSON $ sizeWordHits res)
  , ("functions" , buildDocHits c $ docHits res)
  , ("words" , buildWordHits $ wordHits res)
  , ("modules", buildTopList mods)
  , ("packages", buildTopList pkgs)
  ]

buildDocHits :: HolCache c => c -> DocHits FunctionInfo -> JSValue
buildDocHits _ dh = JSArray $ map buildDoc (IM.toList dh)

buildDoc :: (Int, (DocInfo FunctionInfo, DocContextHits)) -> JSValue
buildDoc (_, (DocInfo (Document t u (Just (FunctionInfo m s p _))) _, _)) = jo
  [ ("name", js $ t)
  , ("uri", js $ u)
  , ("module", js $ B.toString $ m)
  , ("signature", js $ B.toString $ s)
  , ("package", js $ B.toString $ p)
  ]

buildWordHits :: WordHits -> JSValue
buildWordHits wh = JSArray $ map (js . fst) (M.toList wh)

buildTopList :: [(String, Int)] -> JSValue
buildTopList = showJSON . map buildTopElem
  where
  buildTopElem (n, c) = jo [("name", js n), ("count", showJSON c) ]

js :: String -> JSValue
js = JSString . toJSString

jo :: [(String, JSValue)] -> JSValue
jo = JSObject . toJSObject
