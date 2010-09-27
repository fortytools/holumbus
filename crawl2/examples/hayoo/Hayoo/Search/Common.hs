-- ----------------------------------------------------------------------------

{- |
  Module     : Hayoo.Search.Common
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

import Text.XML.HXT.Core	( XmlTree )

-- Status information of query processing (status message, result, top modules, top packages).

type StatusResult 		= (String, Result FunctionInfo, Result PackageInfo, [(String, Int)], [(String, Int)])

-- ----------------------------------------------------------------------------
