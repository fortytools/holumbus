module Holumbus.Data.PrefixTreeTest
where


import           Holumbus.Data.PrefixTree
import           Prelude 	hiding ( succ, lookup, map, null )
import           Test.HUnit
import           System

-- ----------------------------------------

type M	= PrefixTree Int

m1, m2, m3, m4, m5	:: M

m1	= empty
m2	= insert ""    42 $ m1
m3	= insert "x"   53 $ m1
m4	= insert "xxx" 64 $ m1
m5	= insert "x"   23 $ m2
m6	= insert "y"   43 $ m3

test1	= TestLabel "simple trees" $
          TestList $
          [ TestCase $ assertEqual "empty" 	Empty m1
          , TestCase $ assertEqual "val"	(Leaf 42) m2
          , TestCase $ assertEqual "single" 	(Single 'x' (Leaf 53)) m3
          , TestCase $ assertEqual "seq" 	(SiSeq  "xxx" (Leaf 64)) m4
          , TestCase $ assertEqual "val"        (Val 42 (Single 'x' (Leaf 23))) m5
          , TestCase $ assertEqual "branch"     (Branch 'x' (Leaf 53) (Single 'y' (Leaf 43))) $ m6
          , TestCase $ assertEqual "delete"	m1 $ delete "x" m3
          , TestCase $ assertEqual "no delete"  m2 $ delete "x" m2
          , TestCase $ assertEqual "delete seq" m1 $ delete "xxx" m4
          , TestCase $ assertEqual "delete y"   m3 $ delete "y"   m3
          ]

test2	= TestLabel "insert / delete" $
          TestList $
          [ TestCase $ assertEqual ("ins/del " ++ k) m (delete k $ insert k 1 $ m)
            | m <- [m1, m2, m3, m4, m5, m6]
            , k <- ["z","zz","zzz","a","aa","aaa","xxxx","xy","yx"]
          ]

main	= do
          c <- runTestTT $ TestList $
               [ test1, test2]
          putStrLn $ show c
          let errs = errors c
	      fails = failures c
          return ()
          -- System.exitWith (codeGet errs fails)

-- ----------------------------------------
