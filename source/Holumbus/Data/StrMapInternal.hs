-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.StrMapInternal
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.4

  This module just exists to ease the testing of the trie internals. 
  For further information, see Holumbus.Data.StrMap.
  
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}

{-# OPTIONS_HADDOCK hide #-}

module Holumbus.Data.StrMapInternal where

import Prelude hiding (succ, lookup, map, null)

import Data.Maybe
import Data.Char
import Data.Binary

import Control.Monad

import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import qualified Data.List as L
import qualified Data.Map as M

import Control.Parallel.Strategies

-- | A map from Strings to values a.
data StrMap a 
  = End !String !a ![StrMap a]
  | Seq !String ![StrMap a] 

-- Just deriving Eq will not work, because equality on the lists of successors takes the order 
-- into account, whereas the order does not matter here.
instance Eq a => Eq (StrMap a) where 
  (==) (End k1 v1 s1) (End k2 v2 s2) = k1 == k2 && v1 == v2 && s1 L.\\ s2 == []
  (==) (Seq k1 s1) (Seq k2 s2)       = k1 == k2 && s1 L.\\ s2 == []
  (==) (Seq _ _) (End _ _ _)         = False
  (==) (End _ _ _) (Seq _ _)         = False
  (/=) m1 m2                         = not (m1 == m2)

-- Compare based on to-/fromList
instance Ord a => Ord (StrMap a) where
  compare m1 m2 = compare (toList m1) (toList m2)

-- Simple instance of Functor.
instance Functor StrMap where
  fmap = map

-- Simple instance of Data.Foldable
instance Foldable StrMap where
  foldr = fold

-- Stolen from Data.IntMap
instance Show a => Show (StrMap a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- Stolen from Data.IntMap
instance Read a => Read (StrMap a) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)

-- Providing strict evaluation for 'StrMap'.
instance NFData a => NFData (StrMap a) where
  rnf (End k v t) = rnf k `seq` rnf v `seq` rnf t
  rnf (Seq k t)   = rnf k `seq` rnf t

-- Provide native binary serialization (not via to-/fromList).
instance (Binary a) => Binary (StrMap a) where
  put (End k v t) = put (0 :: Word8) >> put k >> put v >> put t 
  put (Seq k t)   = put (1 :: Word8) >> put k >> put t

  get = do tag <- getWord8
           case tag of
             0 -> liftM3 End get get get
             1 -> liftM2 Seq get get
             _ -> fail "Error while decoding StrMap"                       

-- | /O(1)/ Create an empty trie.
empty :: StrMap a
empty = Seq "" []

-- | /O(1)/ Is the map empty?
null :: StrMap a -> Bool
null (Seq _ [])    = True
null (Seq _ (_:_)) = False
null (End _ _ _)   = error "Root node should be Seq!"

-- | /O(1)/ Create a map with a single element.
singleton :: String -> a -> StrMap a
singleton k v = Seq "" [End k v []]

-- | /O(1)/ Extract the key of a node
key :: StrMap a -> String
key (End k _ _) = k
key (Seq k _)   = k

-- | /O(1)/ Extract the value of a node (if there is one)
value :: Monad m => StrMap a -> m a
value (End _ v _) = return v
value (Seq _ _) = fail "No value at this node"

-- | /O(1)/ Extract the value of a node or return a default value if no value exists.
valueWithDefault :: a -> StrMap a -> a
valueWithDefault _ (End _ v _) = v
valueWithDefault d (Seq _ _) = d

-- | /O(1)/ Extract the successors of a node
succ :: StrMap a -> [StrMap a]
succ (End _ _ t) = t
succ (Seq _ t)   = t

-- | /O(1)/ Sets the key of a node.
setKey :: String -> StrMap a -> StrMap a
setKey k (End _ v t) = End k v t
setKey k (Seq _ t)   = Seq k t

-- | /O(1)/ Sets the successors of a node.
setSucc :: [StrMap a] -> StrMap a -> StrMap a
setSucc t (End k v _) = End k v t
setSucc t (Seq k _)   = Seq k t

-- | /O(min(n,L))/ Find the value at a key. Calls error when the element can not be found.
(!) :: StrMap a -> String -> a
(!) m k = if isNothing r then error ("Key " ++ k ++ " is not an element of the map!")
          else fromJust r
          where r = lookup k m

-- | /O(min(n,L))/ Is the key a member of the map?
member :: String -> StrMap a -> Bool
member k m = maybe False (\_ -> True) (lookup k m)

-- | /O(min(n,L))/ Delete an element from the map. If no element exists for the key, the map 
-- remains unchanged.
delete :: String -> StrMap a -> StrMap a
delete = (fromMaybe empty .) . delete'

-- | The internal delete function.
delete' :: String -> StrMap a -> Maybe (StrMap a)
delete' d n | dr == "" && kr == "" = deleteNode n
            | dr /= "" && kr == "" = Just (mergeNode n (mapMaybe (delete' dr) (succ n)))
            | otherwise            = Just n
            where (_, dr, kr) = split d (key n)

-- | Merge a node with its successor if only one successor is left.
mergeNode :: StrMap a -> [StrMap a] -> StrMap a
mergeNode (End k v _) t = End k v t
mergeNode (Seq k _) [t] = if k /= "" then setKey (k ++ (key t)) t else Seq k [t]
mergeNode (Seq k _) t   = Seq k t

-- | Delete a node by either merging it with its successors or removing it completely.
deleteNode :: StrMap a -> Maybe (StrMap a)
deleteNode (End _ _ [])  = Nothing
deleteNode (End k _ [t]) = Just (setKey (k ++ key t) t)
deleteNode (End k _ t)   = Just (Seq k t)
deleteNode n             = Just n

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f key new_value old_value@ will be inserted.
insertWithKey :: (String -> a -> a -> a) -> String -> a -> StrMap a -> StrMap a
insertWithKey f nk nv n = insert' f nk nv nk n

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f new_value old_value@ will be inserted.
insertWith :: (a -> a -> a) -> String -> a -> StrMap a -> StrMap a
insertWith f nk nv n = insert' (\_ new old -> f new old) nk nv nk n

-- | /O(min(n,L))/ Insert a new key and value into the map. If the key is already present in
-- the map, the associated value will be replaced with the new value.
insert :: String -> a -> StrMap a -> StrMap a
insert nk nv n = insertWith const nk nv n

-- | The internal insert function which does the real work. The original new key has to
-- be put through because otherwise it will be shortened on every recursive call.
insert' :: (String -> a -> a -> a) -> String -> a -> String -> StrMap a -> StrMap a
insert' f nk nv ok n | nk == ""             = error "Empty key!"
                     -- Key already exists, the current value will be replaced with the new value.
                     | cr == "" && nr == "" = End s (maybe nv (f ok nv) (value n)) (succ n) 
                     -- Insert into list of successors.
                     | cr == "" && nr /= "" = setSucc (insertSub f nr nv ok (succ n)) n
                     -- New intermediate End node with the new value and the current node with the
                     -- remainder of the key as successor.
                     | nr == "" && cr /= "" = End s nv [setKey cr n]
                     -- New intermediate Seq node which shares the prefix of the new key and the 
                     -- key of the current node.
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
splitNoCase a b = split (L.map toLower a) (L.map toLower b)

-- | /O(n)/ Returns all values.
elems :: StrMap a -> [a]
elems t   = L.map snd (toList t)

-- | /O(n)/ Creates a trie from a list of key\/value pairs.
fromList :: [(String, a)] -> StrMap a
fromList xs = L.foldl' (\p (k, v) -> insert k v p) empty xs

-- | /O(n)/ Returns all elements as list of key value pairs,
toList :: StrMap a -> [(String, a)]
toList = foldWithKey (\k v r -> (k, v):r) []

-- | /O(n)/ The number of elements.
size :: StrMap a -> Int
size = fold (\_ r -> r + 1) 0

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.
prefixFind :: String -> StrMap a -> [a] 
prefixFind q n = L.map snd (prefixFindInternal split q n)

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key and include the keys 
-- in the result.
prefixFindWithKey :: String -> StrMap a -> [(String, a)]
prefixFindWithKey = prefixFindInternal split

-- | /O(max(L,R))/ Same as prefixFind, but case insensitive.
prefixFindNoCase :: String -> StrMap a -> [a]
prefixFindNoCase q n = L.map snd (prefixFindInternal splitNoCase q n)

-- | /O(max(L,R))/ Same as prefixFindWithKey, but case insensitive
prefixFindNoCaseWithKey :: String -> StrMap a -> [(String, a)]
prefixFindNoCaseWithKey = prefixFindInternal splitNoCase

-- | Internal prefix find function which is used to implement every other prefix find function.
prefixFindInternal :: (String -> String -> (String, String, String)) -> String -> StrMap a -> [(String, a)]
prefixFindInternal f = prefixFindInternal' f ""
  where
    prefixFindInternal' sf a p n | pr == ""  = L.map (\(k, v) -> (a ++ k, v)) (toList n)
                                 | kr == ""  = concat (L.map (prefixFindInternal' sf (a ++ (key n)) pr) (succ n))
                                 | otherwise = []
                                 where (_, pr, kr) = sf p (key n)

-- | /O(min(n,L))/ Find the value associated with a key. The function will @return@ the result in
-- the monad or @fail@ in it if the key isn't in the map.
lookup :: Monad m => String -> StrMap a -> m a
lookup q n = case lookup' q n of
             Just v -> return v
             Nothing -> fail "Holumbus.Data.StrMapInternal: Key not found"

-- | Internal lookup function which is generalised for arbitrary monads above.
lookup' :: String -> StrMap a -> Maybe a
lookup' q n | pr == "" = if kr == "" then value n else Nothing
            | kr == "" = let xs = (filter isJust (L.map (lookup pr) (succ n))) in
                         if L.null xs then Nothing else head xs
            | otherwise = Nothing
            where (_, pr, kr) = split q (key n)

-- | /O(max(L,R))/ Search for values matching a key case insensitive.
lookupNoCase :: String -> StrMap a -> [a]
lookupNoCase q n | pr == "" = if kr == "" then maybeToList (value n) else []
                 | kr == "" = concat (L.map (lookupNoCase pr) (succ n))
                 | otherwise = []
                 where (_, pr, kr) = splitNoCase q (key n)

-- | /O(min(n,L))/ Find the value associated with a key or return a default value if nothing 
-- was found.
findWithDefault :: a -> String -> StrMap a -> a
findWithDefault d q n = maybe d id (lookup q n)

-- | /O(n)/ Fold over all key\/value pairs in the map.
foldWithKey :: (String -> a -> b -> b) -> b -> StrMap a -> b
foldWithKey f n m = fold' "" m n
  where
  fold' ck (End k v t) r = let nk = ck ++ k in foldr (fold' nk) (f nk v r) t
  fold' ck (Seq k t) r   = let nk = ck ++ k in foldr (fold' nk) r t

-- | /O(n)/ Fold over all values in the map.
fold :: (a -> b -> b) -> b -> StrMap a -> b
fold f = foldWithKey (\_ v r -> f v r)

-- | /O(n)/ Map over all key\/value pairs in the map.
mapWithKey :: (String -> a -> b) -> StrMap a -> StrMap b
mapWithKey f m = map' "" m
  where
  map' ck (End k v t) = let nk = ck ++ k in End k (f nk v) (L.map (map' nk) t)
  map' ck (Seq k t)   = let nk = ck ++ k in Seq k (L.map (map' nk) t)

-- | /O(n)/ Map over all values in the map.
map :: (a -> b) -> StrMap a -> StrMap b
map f = mapWithKey (\_ v -> f v)

-- | /O(n)/ Convert into an ordinary map.
toMap :: StrMap a -> M.Map String a
toMap = foldWithKey M.insert M.empty

-- | /O(n)/ Convert an ordinary map into a StrMap.
fromMap :: M.Map String a -> StrMap a
fromMap = M.foldWithKey insert empty
