-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.StrMap
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.4

  An efficient implementation of maps from string keys to arbitrary values, 
  used for the Holumbus indexes.

  Values can associated with a string key. Searching for keys is very fast, but
  the trie probably consumes more memory. The main difference to "Data.Map" are the special
  'prefixFind' functions, which can be used to perform prefix queries.

  Most other function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.
  
  > import Holumbus.Data.StrMap (StrMap)
  > import qualified Holumbus.Data.StrMap as SM

  See also
  
    * Donald R. Morrison, 
      \"/PATRICIA - Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
      Journal of the ACM, 15 (4), 1968, pages 514-534.
  
  Many functions have a worst-case complexity of /O(min(n,L))/. This means that the operation
  can become linear with the number of elements with a maximum of /L/, the length of the
  key (the number of characters in the string). The functions for searching a prefix have a
  worst-case complexity of /O(max(L,R))/. This means that the operation can become linear with
  /R/, the number of elements found for the prefix, with a minimum of /L/.

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
  , findWithDefault  
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
  , delete

  -- * Traversal
  , map
  , mapWithKey
  , fold
  , foldWithKey

  -- * Conversion
  , elems
  , toList
  , fromList
  , toMap
  , fromMap
  )
where

import Prelude hiding (lookup, map, null)
import Holumbus.Data.StrMapInternal
