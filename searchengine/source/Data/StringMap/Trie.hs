{-# OPTIONS -fno-warn-unused-imports #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Trie
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  Facade for trie implementation hiding internal functions and constructors
  
-}

-- ----------------------------------------------------------------------------

module Data.StringMap.Trie
    ( StringTrie
    , length
    , check
    , space
    , keyChars
    )
where

import Data.StringMap.Trie.String

-- ----------------------------------------------------------------------------
