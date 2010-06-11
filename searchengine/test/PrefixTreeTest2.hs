{-# OPTIONS 
    -fno-warn-missing-signatures 
    -fno-warn-unused-matches 
    -fno-warn-type-defaults
    -fno-warn-name-shadowing
#-}

module PrefixTreeTest2
where


import           Data.Binary            ( encode
                                        , decode
                                        )
import           Data.List              ( permutations
                                        , tails
                                        , sort
                                        , isPrefixOf
                                        , foldl'
                                        )
import qualified Data.List              as L

import           Holumbus.Data.PrefixTree
import           Holumbus.Data.PrefixTreeCore   ( deepNorm, stat )
import           Prelude                hiding  ( succ, lookup, map, null )
import           Test.HUnit

-- ----------------------------------------

allTests        = TestLabel "PrefixTreeTest2" $
                  TestList $
                  [ test1
                  , test2
                  , test3a, test3b, test3c
                  , test4a, test4b
                  , test5
                  , test6a, test6b
                  ]

-- ----------------------------------------

type M  = PrefixTree Int

m1, m2, m3, m4, m5      :: M

m1      = empty
m2      = insert ""    42 $ m1
m3      = insert "x"   53 $ m1
m4      = insert "xxx" 64 $ m1
m5      = insert "x"   23 $ m2
m6      = insert "y"   43 $ m3
m7      = insert "aaa" 44 $ insert "bbb" 45 $ empty

test1   = TestLabel "simple trees" $
          TestList $
          [ TestCase $ assertEqual "empty"      Empty m1
          , TestCase $ assertEqual "val"        (Leaf 42)                       m2
          , TestCase $ assertEqual "single"     (LsVal 'x' 53)                  m3              -- (Last 'x' (Leaf 53)) m3
          , TestCase $ assertEqual "seq"        (LsSeL "xxx" 64)                m4              -- (LsSeq  "xxx" (Leaf 64)) m4
          , TestCase $ assertEqual "val"        (Val 42 (LsVal 'x' 23))         m5              -- (Val 42 (Last 'x' (Leaf 23))) m5
          , TestCase $ assertEqual "branch"     (BrVal 'x' 53 (LsVal 'y' 43))   m6              -- (Branch 'x' (Leaf 53) (Last 'y' (Leaf 43))) $ m6
          , TestCase $ assertEqual "BrSeq"      (BrSeL "aaa" 44 (LsSeL "bbb" 45)) m7            -- (BrSeq "aaa" (Leaf 44) (LsSeq "bbb" (Leaf 45))) $ m7 
          , TestCase $ assertEqual "delete"     m1 $ delete "x" m3
          , TestCase $ assertEqual "no delete"  m2 $ delete "x" m2
          , TestCase $ assertEqual "delete seq" m1 $ delete "xxx" m4
          , TestCase $ assertEqual "delete y"   m3 $ delete "y"   m3
          , TestCase $ assertEqual "keys"       (sort ws) $ keys ts
          , TestCase $ assertEqual "prefix"     (sort ws) $ prefixFind "" ts
          , TestCase $ assertEqual "prefix b"   (filter ("b" `isPrefixOf`) (sort ws)) $ prefixFind "b" ts
          ]

test2   = TestLabel "insert / delete" $
          TestList $
          [ TestCase $ assertEqual ("ins/del " ++ k) m (delete k $ insert k 1 $ m)
            | m <- [m1, m2, m3, m4, m5, m6]
            , k <- ["z","zz","zzz","a","aa","aaa","xxxx","xy","yx"]
          ]

-- ----------------------------------------

type T          = PrefixTree String

testIns ws      = TestList $
                  [ TestLabel "insert" $
                    TestList $
                    [ TestCase $ assertEqual ("insert " ++ show ws) (head ts) t1
                      | t1 <- tail ts
                    ]
                  , TestLabel "lookup" $
                    TestList $
                    [ TestCase $ assertEqual ("lookup " ++ show w1) (Just w1) (lookup w1 t1)
                      | t1 <- ts
                      , w1 <- ws
                    ]
                  , TestLabel "delete" $
                    TestList $
                    [ TestCase $ assertEqual ("delete " ++ show ws) empty (del t1 ws)
                      | t1 <- ts
                    ]
                  ]
    where
    ins         = foldl (\ t w -> insert w w t) empty
    del t0      = foldl (\ t w -> delete w t) t0
    ts          = fmap ins . permutations $ ws


testMap f ws    = TestLabel "map" $
                  TestList $
                  [ TestCase $ assertEqual ("map" ++ show ws1) (map f $ ins $ fmap (\x -> (x,x)) $ ws1) (ins $ fmap (\x -> (x, f x)) $ ws1) 
                    | ws1 <- tails ws
                  ]
    where
    ins         = foldl (\ t (x, y) -> insert x y t) empty

testSize ws     = TestLabel "size" $
                  TestList $
                  [ TestCase $ assertEqual ("size " ++ show ws1) (length ws1) (size . fromList . fmap (\ x -> (x,x)) $ ws1)
                    | ws1 <- tails ws
                  ]


testBinary ws   = TestLabel "put/get" $
                  TestList $
                  [ TestCase $ assertEqual ("put/get " ++ show ws1) ts1 (decode . encode $ ts1)
                    | ws1 <- tails ws
                    , ts1 <- [mktree ws1]
                  ]

mktree          = foldl' (\ t x -> insert x x t) empty

ws              = ["","B","b","C","bb","bbbbb","bbc"]
ts              = mktree ws

ws2             = concat . fmap tails . words $ "hahshs djajajshdhdh dhh1223344 56djdjdd hhjdjd xxxxxx hahaahhahah ahahahha"
ts2             = mktree ws2

test3a          = testIns ws
test3b          = testIns ["a","b","c","aa","ac","cccc","cccd"]
test3c          = testIns ["","x","xx","xxx","xxxx"]

test4a          = testMap reverse ws
test4b          = testMap reverse ["a","b","c","aa","ac","cccc","cccd","","xxx", "xxyyy"]

test5           = testSize  ["a","b","c","aa","ac","cccc","cccd","","xxx", "xxyyy"]

test6a          = testBinary ws
test6b          = testBinary ["a","b","c","aa","ac","cccc","cccd","","xxx", "xxyyy"]
{-
test7		:: PrefixTree a -> Test
test7 t         = TestLabel "words" $
                  TestList $
                  [ TestCase $ assertEqual "size" 1 (size t)
                  , TestCase $ assertEqual "space" 1 (space t)
                  , TestCase $ assertEqual "norm"  1 (space $ deepNorm t)
                  , TestCase $ assertEqual "stat" [] (toList . stat $ t)
                  ]
-}
-- ----------------------------------------
