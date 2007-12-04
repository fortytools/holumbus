-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Data.Patricia
   Copyright  : Copyright (C) 2007 Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   A patricia trie implementation used for the Spoogle indexes.

-}

-- ----------------------------------------------------------------------------

-- | This module provides an implementation of a trie with string values as keys.
module Spoogle.Data.Patricia where

import Prelude hiding (succ)

data Pat a 
  = End String a [Pat a]
  | Seq String [Pat a] deriving Show

-- | Create an empty trie.
empty :: Pat a
empty = Seq "" []

-- | Extract the key of a node
key :: Pat a -> String
key (End k _ _) = k
key (Seq k _)   = k

-- | Extract the successors of a node
succ :: Pat a -> [Pat a]
succ (End _ _ t) = t
succ (Seq _ t)   = t

setKey :: String -> Pat a -> Pat a
setKey k (End _ v t) = End k v t
setKey k (Seq _ t)   = Seq k t

setSucc :: [Pat a] -> Pat a -> Pat a
setSucc t (End k v _) = End k v t
setSucc t (Seq k _)   = Seq k t

-- | Insert a new key with an associated value in the trie.
insert :: String -> a -> Pat a -> Pat a
insert nk nv n | nk == ""             = error "Empty key!"
               | cr == "" && nr == "" = End s nv (succ n)
               | cr == "" && nr /= "" = setSucc (insertSub nr nv (succ n)) n
               | nr == "" && cr /= "" = End s nv [setKey cr n]
               | otherwise = Seq s [setKey cr n, (End nr nv [])]
               where (s, nr, cr) = split nk (key n)

insertSub :: String -> a -> [Pat a] -> [Pat a]
insertSub k v t = insertSub' k v t []
  where
    insertSub' :: String -> a -> [Pat a] -> [Pat a] -> [Pat a]
    insertSub' nk nv [] r     = (End nk nv []):r
    insertSub' nk nv (x:xs) r = if head (key x) == head nk then (insert nk nv x):r ++ xs else 
                                insertSub' nk nv xs (x:r)

split :: String -> String -> (String, String, String)
split a b = split' a b ("","", "")
  where
    split' :: String -> String -> (String, String, String) -> (String, String, String)
    split' n [] (p, nr, hr) = (p, nr ++ n, hr)
    split' [] h (p, nr, hr) = (p, nr, hr ++ h)
    split' (n:ns) (h:hs) (p, nr, hr) = if n == h then split' ns hs (p ++ [n], nr, hr) else
                                       (p, n:ns, h:hs)

-- | Return all elements of the trie.
elems :: Pat a -> [a]
elems t   = map snd (toList t)

toList :: Pat a -> [(String, a)]
toList n = toList' "" n []
  where
    toList' :: String -> Pat a -> [(String, a)] -> [(String, a)]
    toList' ck (End k v t) r = let nk = ck ++ k in foldr (toList' nk) ((nk, v):r) t
    toList' ck (Seq k t) r   = let nk = ck ++ k in foldr (toList' nk) r t 

size :: Pat a -> Int
size n = size' n 0
  where
    size' :: Pat a -> Int -> Int
    size' (End _ _ t) i = foldr size' (i + 1) t
    size' (Seq _ t) i   = foldr size' i t

-- | Find all values where the string is a prefix of their keys.
prefixFind :: String -> Pat a -> [a] 
prefixFind p n | pr == ""  = elems n
               | kr == ""  = concat (map (prefixFind pr) (succ n))
               | otherwise = []
               where (_, pr, kr) = split p (key n)

-- prefixFind :: String -> Pat a -> [a]
-- prefixFind p n = elems (prefixTree p n)
-- 
-- prefixTree :: String -> Pat a -> Pat a
-- prefixTree p n | pr == ""  = n
--                | kr == ""  = let s = findSub pr (succ n) in if (isNothing s) then empty else prefixTree pr (fromJust s)
--                | otherwise = empty
--                where (_, pr, kr) = split p (key n)
-- 
-- findSub :: String -> [Pat a] -> Maybe (Pat a)
-- findSub _ []     = Nothing
-- findSub k (x:xs) = if head k == head (key x) then Just x else findSub k xs

{-

-- | Find the value associated with a key.
find :: String -> Pat a -> Maybe a
find [] (WordEnd v _)     = Just v
find [] (Node _)          = Nothing
find (x:xs) (WordEnd _ s) = if Map.member x s then find xs (s Map.! x) else Nothing
find (x:xs) (Node s)      = if Map.member x s then find xs (s Map.! x) else Nothing

-}
