-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.StrMap
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A patricia trie implementation used for the Holumbus indexes.

  Arbitrary values can associated with a string key. Searching for keys is very fast,
  but the trie consumes quite some memory. The main difference to Data.Map are the special
  \"prefixFind\" functions, which can be used to perform prefix queries. 

-}

-- ----------------------------------------------------------------------------

module Holumbus.Data.StrMap 
  (
  -- * Map type
  StrMap

  -- * Operators
  , (!)

  -- * Query
  , null
  , size
  , member
  , lookup
  , lookupNoCase
  , prefixFind
  , prefixFindWithKey
  , prefixFindNoCase
  , prefixFindNoCaseWithKey

  -- * Construction
  , empty
  , singleton
  , insert
  , insertWith
  , insertWithKey

  -- * Traversal
  , map
  , mapWithKey
  , fold
  , foldWithKey

  -- * Conversion
  , elems
  , toList
  , fromList
  )
where

import Prelude hiding (lookup, map, null)
import Holumbus.Data.StrMapInternal