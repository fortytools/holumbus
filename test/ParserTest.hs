-- ----------------------------------------------------------------------------

{- |
  Module     : ParserTest
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The some unit tests for the Holumbus query parser.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS 
    -fno-warn-orphans 
    -fno-warn-missing-signatures 
    -fno-warn-missing-methods 
    -fno-warn-unused-matches 
    -fno-warn-type-defaults
#-}

module ParserTest (allTests, allProperties) where

import Data.List
import Data.Char

import Control.Monad

import qualified Holumbus.Query.Parser as P
import Holumbus.Query.Language

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Batch

a :: Query -> Query -> Query
a = BinQuery And

o :: Query -> Query -> Query
o = BinQuery Or

n :: Query -> Query
n = Negation

w :: String -> Query
w = Word

p :: String -> Query
p = Phrase

s :: [String] -> Query -> Query
s = Specifier

cw :: String -> Query
cw = CaseWord

cp :: String -> Query
cp = CasePhrase

fw :: String -> Query
fw = FuzzyWord

andTests :: Test
andTests = TestList
  [ TestCase (assertEqual "Simple two term 'and' query"
  (Right (a (w "abc") (w "def")))
  (P.parseQuery "abc def"))

  , TestCase (assertEqual "Concatenating 'and' terms"
  (Right (a (w "abc") (a (w "def") (w "ghi"))))
  (P.parseQuery "abc def ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  (Right (a (w "abc") (a (w "def") (a (w "ghi") (w "jkl")))))
  (P.parseQuery " \rabc \r  def  \tghi \njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  (Right (a (s ["wurst"] (w "abc")) (a (w "def") (a (w "ghi") (s ["wurst"] (w "jkl"))))))
  (P.parseQuery "wurst:abc def ghi wurst:jkl"))

  , TestCase (assertEqual "Confusing operator"
  (Right (a (w "Apple") (a (w "Anna") (w "ANDroid"))))
  (P.parseQuery "Apple Anna ANDroid"))

  , TestCase (assertEqual "Explicit operator"
  (Right (a (w "abc") (w "def")))
  (P.parseQuery "abc AND def"))
  ]

orTests :: Test
orTests = TestList
  [ TestCase (assertEqual "Simple two term 'or' query"
  (Right (o (w "abc") (w "def")))
  (P.parseQuery "abc OR def"))

  , TestCase (assertEqual "Concatenating 'or' terms"
  (Right (o (w "abc") (o (w "def") (w "ghi"))))
  (P.parseQuery "abc OR def OR ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  (Right (o (w "abc") (o (w "def") (o (w "ghi") (w "jkl")))))
  (P.parseQuery " \rabc \rOR  def OR \tghi OR\njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  (Right (o (s ["wurst"] (w "abc")) (o (w "def") (o (w "ghi") (s ["wurst"] (w "jkl"))))))
  (P.parseQuery "wurst:abc OR def OR ghi OR wurst:jkl"))

  , TestCase (assertEqual "Operator precedence"
  (P.parseQuery "wurst:abc (def OR ghi) wurst:jkl")
  (P.parseQuery "wurst:abc def OR ghi wurst:jkl"))

  , TestCase (assertEqual "Confusing operator"
  (Right (a (w "Operation") (w "ORganism")))
  (P.parseQuery "Operation ORganism"))
  ]
  
specifierTests :: Test
specifierTests = TestList
  [ TestCase (assertEqual "Specifier with whitespace"
  (Right (a (s ["wurst"] (w "abc")) (s ["batzen"] (w "def"))))
  (P.parseQuery " wurst:\t abc \nbatzen : \r def "))

  , TestCase (assertEqual "Specifier priority"
  (Right (a (w "abc") (a (s ["wurst"] (w "def")) (o (n (s ["wurst"] (w "ghi"))) (s ["wurst"] (w "jkl"))))))
  (P.parseQuery "abc wurst: def NOT wurst: ghi OR wurst: jkl"))

  ,TestCase (assertEqual "Specifier and brackets"
  (Right (a (s ["wurst"] (a (w "abc") (a (w "def") (w "ghi")))) (s ["batzen"] (o (w "abc") (w "def")))))
  (P.parseQuery "wurst: (abc def ghi) batzen: (abc OR def)"))

  ,TestCase (assertEqual "Specifier and brackets"
  (Right (a (s ["wurst"] (a (w "abc") (a (w "def") (w "ghi")))) (s ["batzen"] (o (w "abc") (w "def")))))
  (P.parseQuery "wurst: (abc def ghi) batzen: (abc OR def)")) 

  ,TestCase (assertEqual "Specifier and space"
  (Right (a (s ["wurst"] (a (w "abc") (a (w "def") (w "ghi")))) (s ["batzen"] (o (w "abc") (w "def")))))
  (P.parseQuery "wurst \t: (abc def ghi) batzen \n : (abc OR def)")) 

  ,TestCase (assertEqual "Specifier lists"
  (Right (s ["wurst","batzen","schinken"] (a (w "abc") (a (w "def") (w "ghi")))))
  (P.parseQuery "wurst,batzen,schinken: (abc def ghi)")) 

  ,TestCase (assertEqual "Specifier lists with space"
  (Right (s ["wurst","batzen","schinken"] (a (w "abc") (a (w "def") (w "ghi")))))
  (P.parseQuery "wurst , \n batzen \t, schinken: (abc def ghi)")) 

  ,TestCase (assertEqual "Specifier lists with phrase"
  (Right (s ["wurst","batzen","schinken"] (p "this is A Test")))
  (P.parseQuery "wurst , \n batzen \t, schinken: \"this is A Test\"")) 
  ]

notTests :: Test
notTests = TestList
  [ TestCase (assertEqual "Simple not query"
  (Right (n (w "batzen")))
  (P.parseQuery "NOT batzen"))

  , TestCase (assertEqual "Operator precedence"
  (Right (a (n (w "batzen")) (w "wurst")))
  (P.parseQuery "NOT batzen wurst"))

  , TestCase (assertEqual "Operator precedence with and"
  (Right (a (w "test") (a (n (w "batzen")) (w "wurst"))))
  (P.parseQuery "test NOT batzen wurst"))

  , TestCase (assertEqual "Operator precedence with or"
  (Right (o (w "test") (o (n (w "batzen")) (w "wurst"))))
  (P.parseQuery "test OR NOT batzen OR wurst"))

  , TestCase (assertEqual "Confusing operator"
  (Right (a (w "Nail") (a (w "NOrthpole") (w "NOTtingham"))))
  (P.parseQuery "Nail NOrthpole NOTtingham"))
  ]

caseTests :: Test
caseTests = TestList
  [ TestCase (assertEqual "Simple case sensitive word"
  (Right (cw "batzen"))
  (P.parseQuery "!batzen"))

  ,TestCase (assertEqual "Simple case sensitive phrase"
  (Right (cp "this is a test"))
  (P.parseQuery "!\"this is a test\"")) 

  ,TestCase (assertEqual "Case sensitive word with whitespace"
  (Right (cw "test"))
  (P.parseQuery " ! test")) 
  ]

parentheseTests :: Test
parentheseTests = TestList
  [ TestCase (assertEqual "Parentheses without effect"
  (P.parseQuery "abc def OR ghi")
  (P.parseQuery "abc (def OR ghi)"))
  
  , TestCase (assertEqual "Parentheses changing priority of OR"
  (Right (a (o (w "abc") (w "def")) (w "ghi")))
  (P.parseQuery "(abc OR def) ghi"))

  , TestCase (assertEqual "Parentheses with whitespace and OR"
  (Right (o (w "abc") (w "def")))
  (P.parseQuery " ( abc OR def ) "))

  , TestCase (assertEqual "Parentheses with whitespace and AND"
  (Right (a (w "abc") (w "def")))
  (P.parseQuery " ( abc def ) "))
  ]
  
fuzzyTests :: Test
fuzzyTests = TestList
  [ TestCase (assertEqual "Simple fuzzy query"
  (Right (fw "test"))
  (P.parseQuery "~test"))

  , TestCase (assertEqual "Fuzzy query with whitespace"
  (Right (fw "test"))
  (P.parseQuery " ~ test"))
  ]

phraseTests :: Test
phraseTests = TestList
  [ TestCase (assertEqual "Ignoring whitespace without case operator"
  (Right (p "wurst schinken batzen"))
  (P.parseQuery "  \t \n \"wurst schinken batzen\" \t "))

  , TestCase (assertEqual "Ignoring whitespace with case operator"
  (Right (cp "wurst schinken batzen"))
  (P.parseQuery "  \t \n ! \"wurst schinken batzen\" \t "))
  ]

instance Arbitrary Char where
  arbitrary     = oneof [choose ('\65', '\90') ,choose ('\97', '\122')]
  coarbitrary c = variant (ord c `rem` 4)

instance Arbitrary Query where
  arbitrary = sized query

query :: Int -> Gen Query
query num | num == 0 = liftM Word word
          | num < 0 = query (abs num)
          | num > 0 = frequency [ (4, liftM Word word)
                                , (1, liftM CaseWord word)
                                , (2, liftM Phrase phrase)
                                , (1, liftM CasePhrase phrase)
                                , (1, liftM FuzzyWord word)
                                , (1, liftM2 Specifier specs subQuery)
                                , (2, liftM Negation subQuery)
                                , (4, liftM3 BinQuery op subQuery subQuery)
                                ]
query _ = error "Error in query generator!"

op = frequency [(3, return (And)), (1, return (Or))]
subQuery = sized (\num -> query (num `div` 2))
specs = sequence [ word | i <- [1..2] ]
word = sequence [ arbitrary | i <- [1..5] ]
phrase = do
         ws <- sequence [ word | i <- [1..3] ]
         return (intercalate " " ws)

showQuery :: (BinOp -> String) -> Query -> String
showQuery _ (Word st) = st
showQuery _ (Phrase st) = "\"" ++ st ++ "\""
showQuery _ (CaseWord st) = "!" ++ st
showQuery _ (CasePhrase st) = "!\"" ++ st ++ "\""
showQuery _ (FuzzyWord st) = "~" ++ st 
showQuery f (Specifier c q) = (intercalate "," c) ++ ":(" ++ (showQuery f q) ++ ")"
showQuery f (Negation q)= "(NOT " ++ (showQuery f q) ++ ")"
showQuery f (BinQuery opr q1 q2) = "(" ++ (showQuery f q1) ++ 
                                   " " ++ (f opr) ++ 
                                   " " ++ (showQuery f q2) ++ ")"

showOpAnd And = "AND"
showOpAnd Or = "OR"
showOpAnd But = error "But not allowed!"

showOpSpace And = " "
showOpSpace Or = "OR"
showOpSpace But = error "But not allowed!"

prop_ParseAnd q = P.parseQuery (showQuery showOpAnd q) == Right q
prop_ParseSpace q = P.parseQuery (showQuery showOpSpace q) == Right q

allProperties :: (String, [TestOptions -> IO TestResult])
allProperties = ("Parser tests",
                [ run prop_ParseAnd
                , run prop_ParseSpace
                ])
  
allTests :: Test
allTests = TestLabel "Parser tests" $
  TestList 
  [ TestLabel "And tests" andTests
  , TestLabel "Or tests" orTests
  , TestLabel "Not tests" notTests
  , TestLabel "Specifier tests" specifierTests
  , TestLabel "Case tests" caseTests
  , TestLabel "Parenthese tests" parentheseTests
  , TestLabel "Phrase tests" phraseTests
  , TestLabel "Fuzzy tests" fuzzyTests
  ]