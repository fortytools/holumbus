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

a :: P.Query -> P.Query -> P.Query
a q1 q2 = P.BinQuery P.And q1 q2

o :: P.Query -> P.Query -> P.Query
o q1 q2 = P.BinQuery P.Or q1 q2

n :: P.Query -> P.Query
n q = P.Negation q

w :: String -> P.Query
w x = P.Word x

s :: String -> P.Query -> P.Query
s c q = P.Specifier c q

andTests :: Test
andTests = TestList
  [ TestCase (assertEqual "Simple two term 'and' query"
  [(a (w "abc") (w "def"), "")]
  (P.parse P.query "abc def"))

  , TestCase (assertEqual "Concatenating 'and' terms"
  [(a (w "abc") (a (w "def") (w "ghi")), "")]
  (P.parse P.query "abc def ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  [(a (w "abc") (a (w "def") (a (w "ghi") (w "jkl"))), "")]
  (P.parse P.query " \rabc \r  def  \tghi \njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  [(a (s "wurst" (w "abc")) (a (w "def") (a (w "ghi") (s "wurst" (w "jkl")))), "")]
  (P.parse P.query "wurst:abc def ghi wurst:jkl"))
  ]

orTests :: Test
orTests = TestList
  [ TestCase (assertEqual "Simple two term 'or' query"
  [(o (w "abc") (w "def"), "")]
  (P.parse P.query "abc OR def"))

  , TestCase (assertEqual "Concatenating 'or' terms"
  [(o (w "abc") (o (w "def") (w "ghi")), "")]
  (P.parse P.query "abc OR def OR ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  [(o (w "abc") (o (w "def") (o (w "ghi") (w "jkl"))), "")]
  (P.parse P.query " \rabc \rOR  def OR \tghi OR\njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  [(o (s "wurst" (w "abc")) (o (w "def") (o (w "ghi") (s "wurst" (w "jkl")))), "")]
  (P.parse P.query "wurst:abc OR def OR ghi OR wurst:jkl"))
  ]
  
specifierTests ::Test
specifierTests = TestList
  [ TestCase (assertEqual "Specifier with whitespace"
  [(a (s "wurst" (w "abc")) (s "batzen" (w "def")) ,"")]
  (P.parse P.query " wurst:\t abc \nbatzen: \r def "))

  , TestCase (assertEqual "Specifier priority"
  [(a (w "abc") (a (s "wurst" (w "def")) (o (n (s "wurst" (w "ghi"))) (s "wurst" (w "jkl")))) ,"")]
  (P.parse P.query "abc wurst: def NOT wurst: ghi OR wurst: jkl"))

  ,TestCase (assertEqual "Specifier and brackets"
  [(a (s "wurst" (a (w "abc") (a (w "def") (w "ghi")))) (s "batzen" (o (w "abc") (w "def"))) ,"")]
  (P.parse P.query "wurst: (abc def ghi) batzen: (abc OR def)"))
  ]

allTests :: Test
allTests = TestLabel "Parser tests" $
  TestList 
  [ TestLabel "And tests" andTests
  , TestLabel "Or tests" orTests
  , TestLabel "Specifier tests" specifierTests
  ]