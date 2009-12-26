{-# OPTIONS -XBangPatterns -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.PrefixTree.FuzzySearch
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  Functions for fuzzy search in a prefix tree
  
-}

-- ----------------------------------------------------------------------------

module Data.StringMap.PrefixTree.FuzzySearch
where

import		 Data.Char
import		 Data.Maybe
import qualified Data.List		as L

import 		 Data.StringMap.PrefixTree.Core
import           Data.StringMap.Class

-- ----------------------------------------

type StringParser	= String -> [(String, String)]
type PrefixSet		= PrefixTree ()

-- ----------------------------------------

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.

instance StringMapFuzzy PrefixTree where
    prefixFindNoCaseWithKey k	= toList . cutPx' (similarKeys kpIgnoreCase k)

    prefixFindNoCase k		= elems . cutPx' (similarKeys kpIgnoreCase k)

    lookupNoCase k		= toList . cutAllPx' (similarKeys kpIgnoreCase k)

-- ----------------------------------------

similarKeys		:: StringParser -> String -> PrefixSet
similarKeys _kp []	= val () empty
similarKeys  kp k	= foldr melt empty $ kp k
    where
    melt (k', r') t'	= union' const (siseq k' (similarKeys kp r')) t'

similarKeys'		:: StringParser -> String -> [String]
similarKeys' kp		= keys . similarKeys kp

-- ----------------------------------------

-- | Identity key parser

kpIdent			:: StringParser
kpIdent []		= []
kpIdent (x:xs)		= [([x], xs)]

-- | Sequential combination of key parsers

kpSeq			:: StringParser -> StringParser -> StringParser
kpSeq kp1 kp2 k		= concatMap (uncurry p2) $ kp1 k
    where
    p2 k' r'		= fmap (\ k'' -> (k'', r')) $ similarKeys' kp2 k'

-- | Key parser for case insignificat keys

kpIgnoreCase		:: StringParser
kpIgnoreCase []		= []
kpIgnoreCase (x:xs)	= fmap (\ c -> ([c], xs)) . L.nub $ [toLower x, toUpper x]


-- | Key parser for german umlaut substituion
-- TODO: extend parser for iso-latin-1

kpUmlaut		:: StringParser
kpUmlaut []		= []
kpUmlaut (x:xs)		= fmap (\ s -> (s,xs)) . ([x]:) . maybeToList . L.lookup x $ esc
    where
    esc = [ ('\196', "Ae")
          , ('\214', "Oe")
          , ('\220', "Ue")
          , ('\228', "ae")
          , ('\246', "oe")
          , ('\252', "ue")
          , ('\223', "ss")
          ]

-- ----------------------------------------

