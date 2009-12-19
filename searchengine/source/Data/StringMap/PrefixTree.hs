{-# OPTIONS -fno-warn-unused-imports #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.PrefixTree
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  Facade for prefix tree implementation
  
-}

-- ----------------------------------------------------------------------------

module Data.StringMap.PrefixTree
    ( PrefixTree
    , space
    , keyChars
    )
where

import Data.StringMap.PrefixTree.Core
import Data.StringMap.PrefixTree.FuzzySearch
