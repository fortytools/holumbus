-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.PrefixTree
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  Facade for prefix tree implementation
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.Data.PrefixTree
    ( PrefixTree (..)
    , Key
    , (!)
    , value
    , valueWithDefault
    , null
    , size
    , member
    , lookup
    , findWithDefault  
    , prefixFind
    , prefixFindWithKey
    , empty
    , singleton
    , insert
    , insertWith
    , insertWithKey
    , delete
    , update
    , updateWithKey
    , map
    , mapWithKey
    , fold
    , foldWithKey
    , union
    , unionWith
    , unionWithKey
    , difference
    , differenceWith
    , differenceWithKey
    , keys
    , elems
    , toList
    , fromList
    , toMap
    , fromMap
    , space
    , keyChars

    , prefixFindNoCaseWithKey
    , prefixFindNoCase
    , lookupNoCase
    )
where

import Prelude 	hiding ( succ, lookup, map, null )

import Holumbus.Data.PrefixTreeCore
import Holumbus.Data.PrefixTreeFuzzySearch
