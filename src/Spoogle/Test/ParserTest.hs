-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Test.ParserTest
   Copyright  : Copyright (C) 2007 Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   The some unit tests for the Spoogle query parser.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Test.ParserTest (allTests) where

import qualified Spoogle.Query.Parser as P

import Test.HUnit

testAnd1 = TestCase (assertEqual "Simple two term 'and' query"
  [(P.BinQuery P.And (P.Word "abc") (P.Word "def"), "")]
  (P.parse P.query "abc def"))

testAnd2 = TestCase (assertEqual "Concatenating 'and' terms"
  [(P.BinQuery P.And (P.Word "abc") (P.BinQuery P.And (P.Word "def") (P.Word "ghi")), "")]
  (P.parse P.query "abc def ghi"))

andTests = TestList
  [ testAnd1
  , testAnd2
  ]

allTests = TestLabel "Parser tests" $
  TestList 
  [ andTests
  ]