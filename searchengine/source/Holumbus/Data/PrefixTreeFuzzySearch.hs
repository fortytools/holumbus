{-# OPTIONS -XBangPatterns #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.PrefixTreeFuzzySearch
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  Functions for fuzzy search in a prefix tree
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.Data.PrefixTreeFuzzySearch
where

import		 Data.Char
import		 Data.Maybe
import qualified Data.List		as L

import 		 Holumbus.Data.PrefixTreeCore

-- ----------------------------------------

type KeyParser		= Key -> [(Key, Key)]
type PrefixSet		= PrefixTree ()

-- ----------------------------------------

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.

prefixFindNoCaseWithKey		:: Key -> PrefixTree a -> [(Key, a)] 
prefixFindNoCaseWithKey k	= toList . cutPx' (similarKeys kpIgnoreCase k)

prefixFindNoCase		:: Key -> PrefixTree a -> [a] 
prefixFindNoCase k		= elems . cutPx' (similarKeys kpIgnoreCase k)

lookupNoCase			:: Key -> PrefixTree a -> [(Key, a)]
lookupNoCase k			= toList . cutAllPx' (similarKeys kpIgnoreCase k)

-- ----------------------------------------

similarKeys		:: KeyParser -> Key -> PrefixSet
similarKeys _kp []	= val () empty
similarKeys  kp k	= foldr melt empty $ kp k
    where
    melt (k', r') t'	= union' const (siseq k' (similarKeys kp r')) t'

similarKeys'		:: KeyParser -> Key -> [Key]
similarKeys' kp		= keys . similarKeys kp

-- ----------------------------------------

-- | Identity key parser

kpIdent			:: KeyParser
kpIdent []		= []
kpIdent (x:xs)		= [([x], xs)]

-- | Sequential combination of key parsers

kpSeq			:: KeyParser -> KeyParser -> KeyParser
kpSeq kp1 kp2 k		= concatMap (uncurry p2) $ kp1 k
    where
    p2 k' r'		= fmap (\ k'' -> (k'', r')) $ similarKeys' kp2 k'

-- | Key parser for case insignificat keys

kpIgnoreCase		:: KeyParser
kpIgnoreCase []		= []
kpIgnoreCase (x:xs)	= fmap (\ c -> ([c], xs)) . L.nub $ [toLower x, toUpper x]


-- | Key parser for german umlaut substituion
-- TODO: extend parser for iso-latin-1

kpUmlaut		:: KeyParser
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

