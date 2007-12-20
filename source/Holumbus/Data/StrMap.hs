-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.StrMap
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A patricia trie implementation used for the Holumbus indexes.

  Arbitrary values can associated with a string key. Searching for keys is very fast,
  but the trie consumes quite some memory. The main difference to Data.Map are the special
  \"prefixFind\" functions, which can be used to perform prefix queries. 

-}

-- ----------------------------------------------------------------------------

module Holumbus.Data.StrMap 
  (
  -- * Map type
  StrMap

  -- * Operators
  , (!)

  -- * Query
  , null
  , size
  , member
  , lookup
  , lookupNoCase
  , prefixFind
  , prefixFindWithKey
  , prefixFindNoCase
  , prefixFindNoCaseWithKey

  -- * Construction
  , empty
  , singleton
  , insert
  , insertWith
  , insertWithKey

  -- * Conversion
  , elems
  , toList
  , fromList
  )
where

import Prelude hiding (succ, lookup)

import Data.Maybe
import Data.Char
import qualified Data.List as L

-- | A map from Strings to values a.
data StrMap a 
  = End String a [StrMap a]
  | Seq String [StrMap a] deriving (Show)

-- | Just deriving Eq will not work, because equality on the lists of successors takes the order 
--   into account, whereas the order does not matter here.
instance (Eq a) => Eq (StrMap a) where 
  (==) (End k1 v1 s1) (End k2 v2 s2) = k1 == k2 && v1 == v2 && s1 L.\\ s2 == []
  (==) (Seq k1 s1) (Seq k2 s2)       = k1 == k2 && s1 L.\\ s2 == []
  (==) (Seq _ _) (End _ _ _)         = False
  (==) (End _ _ _) (Seq _ _)         = False
  (/=) m1 m2                         = not (m1 == m2)

-- | Create an empty trie.
empty :: StrMap a
empty = Seq "" []

-- | Create a map with a single element.
singleton :: String -> a -> StrMap a
singleton k v = Seq "" [End k v []]

-- | Extract the key of a node
key :: StrMap a -> String
key (End k _ _) = k
key (Seq k _)   = k

-- | Extract the value of a node (if there is one)
value :: StrMap a -> Maybe a
value (End _ n _) = Just n
value (Seq _ _) = Nothing

-- | Extract the successors of a node
succ :: StrMap a -> [StrMap a]
succ (End _ _ t) = t
succ (Seq _ t)   = t

-- | Sets the key of a node.
setKey :: String -> StrMap a -> StrMap a
setKey k (End _ v t) = End k v t
setKey k (Seq _ t)   = Seq k t

-- | Sets the successors of a node.
setSucc :: [StrMap a] -> StrMap a -> StrMap a
setSucc t (End k v _) = End k v t
setSucc t (Seq k _)   = Seq k t

-- | Find the value at a key. Calls error when the element can not be found.
(!) :: StrMap a -> String -> a
(!) m k = if isNothing r then error ("key " ++ k ++ " is not an element of the map")
          else fromJust r
          where r = lookup k m

-- | Is the key a member of the map?
member :: String -> StrMap a -> Bool
member k m = maybe False (\_ -> True) (lookup k m)

-- | Insert with a combining function. If the key is already present in the map, the
-- value of @f key new_value old_value@ will be inserted.
insertWithKey :: (String -> a -> a -> a) -> String -> a -> StrMap a -> StrMap a
insertWithKey f nk nv n = insert' f nk nv nk n

-- | Insert with a combining function. If the key is already present in the map, the
-- value of @f new_value old_value@ will be inserted.
insertWith :: (a -> a -> a) -> String -> a -> StrMap a -> StrMap a
insertWith f nk nv n = insert' (\_ new old -> f new old) nk nv nk n

-- | Insert a new key and value into the map. If the key is already present in the map,
-- the associated value will be replaced with the new value.
insert :: String -> a -> StrMap a -> StrMap a
insert nk nv n = insertWith const nk nv n

-- | The internal insert function which does the real work. The original new key has to be put
-- through because otherwise it will be shortened on every recursive call.
insert' :: (String -> a -> a -> a) -> String -> a -> String -> StrMap a -> StrMap a
insert' f nk nv ok n | nk == ""             = error "Empty key!"
                     -- Key already exists, the current value will be replaced with the new value.
                     | cr == "" && nr == "" = End s (maybe nv (f ok nv) (value n)) (succ n) 
                     -- Insert into list of successors.
                     | cr == "" && nr /= "" = setSucc (insertSub f nr nv ok (succ n)) n
                     -- New intermediate End node with the new value and the current node as successor.
                     | nr == "" && cr /= "" = End s nv [setKey cr n]
                     -- New intermediate Seq node which shares the prefix of the new key and the key of the current node.
                     | otherwise = Seq s [setKey cr n, (End nr nv [])]
                     where (s, nr, cr) = split nk (key n)

-- | Internal support function for insert which searches the correct successor to insert into
-- within a list of nodes (the successors of the current node, see call in insert' above).
insertSub :: (String -> a -> a -> a) -> String -> a -> String -> [StrMap a] -> [StrMap a]
insertSub f k v o t = insertSub' f k v t []
  where
    insertSub' :: (String -> a -> a -> a) -> String -> a -> [StrMap a] -> [StrMap a] -> [StrMap a]
    insertSub' _ nk nv [] r     = (End nk nv []):r
    insertSub' cf nk nv (x:xs) r = if head (key x) == head nk then (insert' cf nk nv o x):r ++ xs else 
                                  insertSub' cf nk nv xs (x:r)

-- | Analyses two strings and splits them into three parts: A common prefix and both reminders
split :: String -> String -> (String, String, String)
split a b = split' a b ("","", "")
  where
    split' :: String -> String -> (String, String, String) -> (String, String, String)
    split' n [] (p, nr, hr) = (p, nr ++ n, hr)
    split' [] h (p, nr, hr) = (p, nr, hr ++ h)
    split' (n:ns) (h:hs) (p, nr, hr) = if n == h then split' ns hs (p ++ [n], nr, hr) else
                                       (p, n:ns, h:hs)

-- | Same a ssplit above, but case insensitive (strings are converted to lower case).
splitNoCase :: String -> String -> (String, String, String)
splitNoCase a b = split (map toLower a) (map toLower b)

-- | Returns all values.
elems :: StrMap a -> [a]
elems t   = map snd (toList t)

-- | Returns all elements as key value pairs,
toList :: StrMap a -> [(String, a)]
toList n = toList' "" n []
  where
    toList' :: String -> StrMap a -> [(String, a)] -> [(String, a)]
    toList' ck (End k v t) r = let nk = ck ++ k in foldr (toList' nk) ((nk, v):r) t
    toList' ck (Seq k t) r   = let nk = ck ++ k in foldr (toList' nk) r t 

-- | Creates a trie from a list of key\/value pairs.
fromList :: [(String, a)] -> StrMap a
fromList xs = foldr (\(k, v) p -> insert k v p) empty xs

-- | The number of elements.
size :: StrMap a -> Int
size n = size' n 0
  where
    size' :: StrMap a -> Int -> Int
    size' (End _ _ t) i = foldr size' (i + 1) t
    size' (Seq _ t) i   = foldr size' i t

-- | Find all values where the string is a prefix of the key.
prefixFind :: String -> StrMap a -> [a] 
prefixFind q n = map snd (prefixFindInternal split q n)

-- | Find all values where the string is a prefix of the key and include the keys in the result.
prefixFindWithKey :: String -> StrMap a -> [(String, a)]
prefixFindWithKey = prefixFindInternal split

-- | Same as prefixFind, but case insensitive.
prefixFindNoCase :: String -> StrMap a -> [a]
prefixFindNoCase q n = map snd (prefixFindInternal splitNoCase q n)

-- | Same as prefixFindWithKey, but case insensitive
prefixFindNoCaseWithKey :: String -> StrMap a -> [(String, a)]
prefixFindNoCaseWithKey = prefixFindInternal splitNoCase

-- | Internal prefix find function which is used to implement every other prefix find function.
prefixFindInternal :: (String -> String -> (String, String, String)) -> String -> StrMap a -> [(String, a)]
prefixFindInternal f = prefixFindInternal' f ""
  where
    prefixFindInternal' sf a p n | pr == ""  = map (\(k, v) -> (a ++ k, v)) (toList n)
                                 | kr == ""  = concat (map (prefixFindInternal' sf (a ++ (key n)) pr) (succ n))
                                 | otherwise = []
                                 where (_, pr, kr) = sf p (key n)

-- | Find the value associated with a key.
lookup :: String -> StrMap a -> Maybe a
lookup q n | pr == "" = if kr == "" then value n else Nothing
           | kr == "" = let xs = (filter isJust (map (lookup pr) (succ n))) in
                        if null xs then Nothing else head xs
           | otherwise = Nothing
           where (_, pr, kr) = split q (key n)

-- | Search for values matching a key case insensitive.
lookupNoCase :: String -> StrMap a -> [a]
lookupNoCase q n | pr == "" = if kr == "" then maybeToList (value n) else []
                 | kr == "" = concat (map (lookupNoCase pr) (succ n))
                 | otherwise = []
                 where (_, pr, kr) = splitNoCase q (key n)
