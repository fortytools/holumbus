-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Search.Common.hs
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Helper functions and types used by the Hayoo crawler and the
  Hayoo web search.

-}

-- ----------------------------------------------------------------------------

module Hayoo.Search.Common where

import Holumbus.Query.Result	( Result )

import Hayoo.IndexTypes		( FunctionInfo, PackageInfo )

import Text.XML.HXT.Arrow	( XmlTree )


-- Status information of query processing (status message, result, top modules, top packages).

type StatusResult a 		= (String, Result a, [(String, Int)], [(String, Int)])

type StatusResultFct		= StatusResult FunctionInfo

type StatusResultPkg		= StatusResult PackageInfo

type Template 			= XmlTree

-- ----------------------------------------------------------------------------
