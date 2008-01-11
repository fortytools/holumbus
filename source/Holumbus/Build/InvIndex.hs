-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Parser
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt (sms@holumbus.schlatt.com)
  Stability  : experimental
  Portability: untested
  Version    : $Id$

  The Holumbus sailor.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Build.InvIndex where

import Text.XML.HXT.Arrow			-- import all stuff for parsing, validating, and transforming XML

import System.IO			   		-- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit
import System

import Holumbus.Data.StrMap (StrMap)
import qualified Holumbus.Data.StrMap as SM

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.IntMap as IM

import qualified Data.IntSet as IS


import Data.Maybe
import Data.List

import Holumbus.Index.Inverted
import Holumbus.Index.Common


-- import MapReduce.Basic
import qualified MapReduce.Basic as MAPRED

import qualified Misc.Fold
import qualified Misc.Unfold

type Dict = Map


r f = readDocument [(a_parse_html ,"1")] f 

{-
r1 = r (testDocs !! 1)
ra = map r testDocs

dest = "/home/sms/workspace/ruby.idx"
idxfile = "/home/sms/haskell/spoogle/www.haskell.org.idx.xml"



stdOpts = [(a_validate, v_0)]

nn src = nn' src stdOpts 

nn' src opts = readDocument opts src















wordDocPair = MAPRED.mapReduce wdpMAP wdpREDUCE

wdpMAP :: a -> String -> [(String, (a, Int))] 
wdpMAP k v =  map (flipFlop k) (zip [1..] (Misc.Fold.words v))
  where flipFlop k (pos, word) = (word, (k, pos))

wdpREDUCE :: String -> [(Int, Int)] -> Maybe (IM.IntMap IS.IntSet)
wdpREDUCE _ l = Just $ foldl theFunc IM.empty l                     -- [(doc, pos)]
  where 
    theFunc im (doc, pos) =
      if IM.member doc im
        then IM.adjust (IS.insert pos) doc im
        else IM.insert doc (IS.singleton pos) im

wdp =
       print
     $ wordDocPair
     $ simple_INTput


simple_INTput =
       M.insert 2 "appreciate the unfold"
     $ M.insert 1 "fold the fold"
     $ M.empty


testDocs ::  [String]
testDocs = ["/home/sms/ruby/ruby0.htm"	
           ,"/home/sms/ruby/ruby1.htm"
           ,"/home/sms/ruby/ruby2.htm"
           ,"/home/sms/ruby/ruby3.htm"
           ,"/home/sms/ruby/ruby4.htm"
           ,"/home/sms/ruby/ruby5.htm"
           ,"/home/sms/ruby/ruby6.htm"
           ]-}
