{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.BasicTypes
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  Basic data types for index

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.BasicTypes
where

import           Data.Word      ( Word32 )

-- ------------------------------------------------------------

-- | The URI describing the location of the original document.
type URI                        = String

-- | The title of a document.
type Title                      = String

-- | The content of a document.
type Content                    = String

-- | The position of a word in the document.
--type Position                 = Int
type Position                   = Word32

-- | The name of a context.
type Context                    = String

-- | A single word.
type Word                       = String

-- ------------------------------------------------------------
