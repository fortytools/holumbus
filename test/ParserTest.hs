-- ----------------------------------------------------------------------------

{- |
  Module     : ParserTest
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the Spoogle query parser.

-}

-- ----------------------------------------------------------------------------

module ParserTest (allTests) where

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
  (P.parseQuery "abc def"))

  , TestCase (assertEqual "Concatenating 'and' terms"
  [(a (w "abc") (a (w "def") (w "ghi")), "")]
  (P.parseQuery "abc def ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  [(a (w "abc") (a (w "def") (a (w "ghi") (w "jkl"))), "")]
  (P.parseQuery " \rabc \r  def  \tghi \njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  [(a (s "wurst" (w "abc")) (a (w "def") (a (w "ghi") (s "wurst" (w "jkl")))), "")]
  (P.parseQuery "wurst:abc def ghi wurst:jkl"))
  ]

orTests :: Test
orTests = TestList
  [ TestCase (assertEqual "Simple two term 'or' query"
  [(o (w "abc") (w "def"), "")]
  (P.parseQuery "abc OR def"))

  , TestCase (assertEqual "Concatenating 'or' terms"
  [(o (w "abc") (o (w "def") (w "ghi")), "")]
  (P.parseQuery "abc OR def OR ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  [(o (w "abc") (o (w "def") (o (w "ghi") (w "jkl"))), "")]
  (P.parseQuery " \rabc \rOR  def OR \tghi OR\njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  [(o (s "wurst" (w "abc")) (o (w "def") (o (w "ghi") (s "wurst" (w "jkl")))), "")]
  (P.parseQuery "wurst:abc OR def OR ghi OR wurst:jkl"))
  ]
  
specifierTests :: Test
specifierTests = TestList
  [ TestCase (assertEqual "Specifier with whitespace"
  [(a (s "wurst" (w "abc")) (s "batzen" (w "def")) ,"")]
  (P.parseQuery " wurst:\t abc \nbatzen: \r def "))

  , TestCase (assertEqual "Specifier priority"
  [(a (w "abc") (a (s "wurst" (w "def")) (o (n (s "wurst" (w "ghi"))) (s "wurst" (w "jkl")))) ,"")]
  (P.parseQuery "abc wurst: def NOT wurst: ghi OR wurst: jkl"))

  ,TestCase (assertEqual "Specifier and brackets"
  [(a (s "wurst" (a (w "abc") (a (w "def") (w "ghi")))) (s "batzen" (o (w "abc") (w "def"))) ,"")]
  (P.parseQuery "wurst: (abc def ghi) batzen: (abc OR def)"))
  ]

parentheseTests :: Test
parentheseTests = TestList
  [ TestCase (assertEqual "Parentheses without effect"
  (P.parseQuery "abc def OR ghi")
  (P.parseQuery "abc (def OR ghi)"))
  
  , TestCase (assertEqual "Parentheses changing priority of OR"
  [(a (o (w "abc") (w "def")) (w "ghi"), "")]
  (P.parseQuery "(abc OR def) ghi"))
  ]
  
allTests :: Test
allTests = TestLabel "Parser tests" $
  TestList 
  [ TestLabel "And tests" andTests
  , TestLabel "Or tests" orTests
  , TestLabel "Specifier tests" specifierTests
  , TestLabel "Parenthese tests" parentheseTests
  ]