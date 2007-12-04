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

module Spoogle.Data.Patricia 
  (Pat (End, Seq), empty, insert, elems, toList, toPat, size, prefixFind, find) 
  where

import Prelude hiding (succ)

import Data.Maybe

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

-- | Extract the value of a node (if there is one)
value :: Pat a -> Maybe a
value (End _ n _) = Just n
value (Seq _ _) = Nothing

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

-- | Returns all values.
elems :: Pat a -> [a]
elems t   = map snd (toList t)

-- | Returns all elements as key value pairs,
toList :: Pat a -> [(String, a)]
toList n = toList' "" n []
  where
    toList' :: String -> Pat a -> [(String, a)] -> [(String, a)]
    toList' ck (End k v t) r = let nk = ck ++ k in foldr (toList' nk) ((nk, v):r) t
    toList' ck (Seq k t) r   = let nk = ck ++ k in foldr (toList' nk) r t 

-- | Creates a trie from a list of key/value pairs.
toPat :: [(String, a)] -> Pat a
toPat xs = foldr (\(k, v) p -> insert k v p) empty xs

-- | The number of elements.
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

-- | Find the value associated with a key.
find :: String -> Pat a -> Maybe a
find q n | pr == "" = if kr == "" then value n else Nothing
         | kr == "" = let xs = (filter isJust (map (find pr) (succ n))) in
                        if null xs then Nothing else head xs
         | otherwise = Nothing
         where (_, pr, kr) = split q (key n)
