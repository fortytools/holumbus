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

module EvalSearch
    ( Core(..)
    )
where

import Data.ByteString.Lazy.Char8       ( ByteString
                                        , pack
                                        , fromChunks
                                        )
import Data.Function
import Data.Maybe

import qualified Data.IntMap            as IM
import qualified Data.IntSet            as IS
import qualified Data.List              as L
import qualified Data.Map               as M
import qualified Data.Text.Encoding     as T

import Data.String.Unicode

import Holumbus.Index.Common

import Holumbus.Query.Language.Grammar
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy

import Holumbus.Utility

import IndexTypes

import Network.URI                      ( unEscapeString )

import System.FilePath                  ( takeExtension )

import qualified
       Text.XHtmlCombinators            as X

import Text.XML.HXT.Core

-- ------------------------------------------------------------

data Core = Core
          { index     :: ! CompactInverted
          , documents :: ! (SmallDocuments PageInfo)
          }

-- ------------------------------------------------------------




