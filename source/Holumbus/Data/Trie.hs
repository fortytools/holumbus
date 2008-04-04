{-# OPTIONS -fglasgow-exts -fno-warn-type-defaults #-}  -- Moving these to the top due to a bug in GHC.

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.Trie
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.6

  An efficient implementation of maps from arbitrary byte key to arbitrary values.

  Values can associated with an arbitrary byte key. Searching for keys is very fast, but
  the trie probably consumes more memory than "Data.Map". The main differences are the special
  'prefixFind' functions, which can be used to perform prefix queries. The interface is
  heavily borrowed from "Data.Map" and "Data.IntMap".

  Most other function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.
  
  > import Holumbus.Data.Trie (Trie)
  > import qualified Holumbus.Data.Trie as T

  See also
  
    * Donald R. Morrison, 
      \"/PATRICIA - Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
      Journal of the ACM, 15 (4), 1968, pages 514-534.
  
  Many functions have a worst-case complexity of /O(min(n,L))/. This means that the operation
  can become linear with the number of elements with a maximum of /L/, the length of the
  key (the number of bytes in the list). The functions for searching a prefix have a worst-case
  complexity of /O(max(L,R))/. This means that the operation can become linear with
  /R/, the number of elements found for the prefix, with a minimum of /L/.
  
  The module exports include the internal data types, their constructors and access
  functions for ultimate flexibility. Derived modules should not export these 
  (as shown in "Holumbus.Data.StrMap") to provide only a restricted interface.
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.Data.Trie 
  (
  -- * Trie types
  Trie (..)
  , Key

  -- * Operators
  , (!)

  -- * Query
  , key
  , succ
  , value
  , valueWithDefault
  , null
  , size
  , member
  , lookup
  , lookupBy
  , findWithDefault  
  , prefixFind
  , prefixFindWithKey
  , prefixFindBy
  , prefixFindWithKeyBy

  -- * Construction
  , empty
  , singleton
  , insert
  , insertWith
  , insertWithKey
  , delete
  , update
  , updateWithKey

  -- * Traversal
  , map
  , mapWithKey
  , fold
  , foldWithKey

  -- * Combine
  , union
  , unionWith
  , unionWithKey
  , difference
  , differenceWith
  , differenceWithKey

  -- * Conversion
  , elems
  , toList
  , fromList
  , toMap
  , fromMap
  
  -- * Debug
  , lengths
  , check
  )
where

import Prelude hiding (succ, lookup, map, null)

import Data.Maybe
import Data.Binary
import Data.Word

import Control.Monad

import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import qualified Data.List as L
import qualified Data.Map as M

import Control.Parallel.Strategies

-- | A map from arbitrary byte keys to values a.
data Trie a = End !Key a ![Trie a]
            | Seq !Key ![Trie a] 

-- | The key type.
type Key = [Word8]

-- Just deriving Eq will not work, because equality on the lists of successors takes the order 
-- into account, whereas the order does not matter here.
instance Eq a => Eq (Trie a) where 
  (==) (End k1 v1 s1) (End k2 v2 s2) = k1 == k2 && v1 == v2 && s1 L.\\ s2 == []
  (==) (Seq k1 s1) (Seq k2 s2)       = k1 == k2 && s1 L.\\ s2 == []
  (==) (Seq _ _) (End _ _ _)         = False
  (==) (End _ _ _) (Seq _ _)         = False
  (/=) m1 m2                         = not (m1 == m2)

-- Compare based on to-/fromList
instance Ord a => Ord (Trie a) where
  compare m1 m2 = compare (toList m1) (toList m2)

-- Simple instance of Functor.
instance Functor Trie where
  fmap = map

-- Simple instance of Data.Foldable
instance Foldable Trie where
  foldr = fold

-- Stolen from Data.IntMap
instance Show a => Show (Trie a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- Stolen from Data.IntMap
instance Read a => Read (Trie a) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)

-- Providing strict evaluation for 'StrMap'.
instance NFData a => NFData (Trie a) where
  rnf (End k v t) = rnf k `seq` rnf v `seq` rnf t
  rnf (Seq k t)   = rnf k `seq` rnf t

-- Provide native binary serialization (not via to-/fromList).
instance (Binary a) => Binary (Trie a) where
  put (End k v t) = put (0 :: Word8) >> put k >> put v >> put t 
  put (Seq k t)   = put (1 :: Word8) >> put k >> put t

  get = do tag <- getWord8
           case tag of
             0 -> liftM3 End get get get
             1 -> liftM2 Seq get get
             _ -> fail "Trie.get: error while decoding StrMap"                       

-- | /O(1)/ Create an empty trie.
empty :: Trie a
empty = Seq [] []

-- | /O(1)/ Is the map empty?
null :: Trie a -> Bool
null (Seq _ [])    = True
null (Seq _ (_:_)) = False
null (End _ _ _)   = error "Trie.null: root node should be Seq"

-- | /O(1)/ Create a map with a single element.
singleton :: Key -> a -> Trie a
singleton k v = Seq [] [End k v []]

-- | /O(1)/ Extract the key of a node
key :: Trie a -> Key
key (End k _ _) = k
key (Seq k _)   = k

-- | /O(1)/ Extract the value of a node (if there is one)
value :: Monad m => Trie a -> m a
value (End _ v _) = return v
value (Seq _ _) = fail "Trie.value: no value at this node"

-- | /O(1)/ Extract the value of a node or return a default value if no value exists.
valueWithDefault :: a -> Trie a -> a
valueWithDefault _ (End _ v _) = v
valueWithDefault d (Seq _ _) = d

-- | /O(1)/ Extract the successors of a node
succ :: Trie a -> [Trie a]
succ (End _ _ t) = t
succ (Seq _ t)   = t

-- | /O(1)/ Sets the key of a node.
setKey :: Key -> Trie a -> Trie a
setKey k (End _ v t) = End k v t
setKey k (Seq _ t)   = Seq k t

-- | /O(1)/ Sets the successors of a node.
setSucc :: [Trie a] -> Trie a -> Trie a
setSucc t (End k v _) = End k v t
setSucc t (Seq k _)   = Seq k t

-- | /O(min(n,L))/ Find the value at a key. Calls error when the element can not be found.
(!) :: Trie a -> Key -> a
(!) m k = if isNothing r then error ("Trie.!: element not in the map")
          else fromJust r
          where r = lookup k m

-- | /O(min(n,L))/ Is the key a member of the map?
member :: Key -> Trie a -> Bool
member k m = maybe False (\_ -> True) (lookup k m)

-- | /O(min(n,L))/ Delete an element from the map. If no element exists for the key, the map 
-- remains unchanged.
delete :: Key -> Trie a -> Trie a
delete = (fromMaybe empty .) . delete'

-- | The internal delete function.
delete' :: Key -> Trie a -> Maybe (Trie a)
delete' d n | L.null dr && L.null kr       = deleteNode n
            | not (L.null dr) && L.null kr = Just (mergeNode n (mapMaybe (delete' dr) (succ n)))
            | otherwise                    = Just n
            where (_, dr, kr) = split d (key n)

-- | Merge a node with its successor if only one successor is left.
mergeNode :: Trie a -> [Trie a] -> Trie a
mergeNode (End k v _) t = End k v t
mergeNode (Seq k _) [t] = if not (L.null k) then setKey (k ++ (key t)) t else Seq k [t]
mergeNode (Seq k _) t   = Seq k t

-- | Delete a node by either merging it with its successors or removing it completely.
deleteNode :: Trie a -> Maybe (Trie a)
deleteNode (End _ _ [])  = Nothing
deleteNode (End k _ [t]) = Just (setKey (k ++ key t) t)
deleteNode (End k _ t)   = Just (Seq k t)
deleteNode n             = Just n

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f key new_value old_value@ will be inserted.
insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> Trie a -> Trie a
insertWithKey f nk nv n = insert' f nk nv nk n

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f new_value old_value@ will be inserted.
insertWith :: (a -> a -> a) -> Key -> a -> Trie a -> Trie a
insertWith f nk nv n = insert' (\_ new old -> f new old) nk nv nk n

-- | /O(min(n,L))/ Insert a new key and value into the map. If the key is already present in
-- the map, the associated value will be replaced with the new value.
insert :: Key -> a -> Trie a -> Trie a
insert nk nv n = insertWith const nk nv n

-- | The internal insert function which does the real work. The original new key has to
-- be put through because otherwise it will be shortened on every recursive call.
insert' :: (Key -> a -> a -> a) -> Key -> a -> Key -> Trie a -> Trie a
insert' f nk nv ok n | L.null nk                    = error "Empty key!"
                     -- Key already exists, the current value will be replaced with the new value.
                     | L.null cr && L.null nr       = End s (maybe nv (f ok nv) (value n)) (succ n) 
                     -- Insert into list of successors.
                     | L.null cr && not (L.null nr) = setSucc (insertSub f nr nv ok (succ n)) n
                     -- New intermediate End node with the new value and the current node with the
                     -- remainder of the key as successor.
                     | L.null nr && not (L.null cr) = End s nv [setKey cr n]
                     -- New intermediate Seq node which shares the prefix of the new key and the 
                     -- key of the current node.
                     | otherwise = Seq s [setKey cr n, (End nr nv [])]
                     where (s, nr, cr) = split nk (key n)

-- | Internal support function for insert which searches the correct successor to insert into
-- within a list of nodes (the successors of the current node, see call in insert' above).
insertSub :: (Key -> a -> a -> a) -> Key -> a -> Key -> [Trie a] -> [Trie a]
insertSub f k v o t = insertSub' f k v t []
  where
    insertSub' :: (Key -> a -> a -> a) -> Key -> a -> [Trie a] -> [Trie a] -> [Trie a]
    insertSub' _ nk nv [] r     = (End nk nv []):r
    insertSub' cf nk nv (x:xs) r = if head (key x) == head nk then (insert' cf nk nv o x):r ++ xs else 
                                  insertSub' cf nk nv xs (x:r)

-- | Analyses two strings and splits them into three parts: A common prefix and both reminders
splitBy :: (Key -> Key) -> Key -> Key -> (Key, Key, Key)
splitBy f a b = splitBy' (f a) (f b) ([], [], [])
  where
    splitBy' :: Key -> Key -> (Key, Key, Key) -> (Key, Key, Key)
    splitBy' n [] (p, nr, hr) = (p, nr ++ n, hr)
    splitBy' [] h (p, nr, hr) = (p, nr, hr ++ h)
    splitBy' (n:ns) (h:hs) (p, nr, hr) = if n == h then splitBy' ns hs (p ++ [n], nr, hr) 
                                         else (p, n:ns, h:hs)

-- | Simple split without any preprocessing.
split :: Key -> Key -> (Key, Key, Key)
split = splitBy id

-- | /O(n)/ Returns all values.
elems :: Trie a -> [a]
elems t   = L.map snd (toList t)

-- | /O(n)/ Creates a trie from a list of key\/value pairs.
fromList :: [(Key, a)] -> Trie a
fromList xs = L.foldl' (\p (k, v) -> insert k v p) empty xs

-- | /O(n)/ Returns all elements as list of key value pairs,
toList :: Trie a -> [(Key, a)]
toList = foldWithKey (\k v r -> (k, v):r) []

-- | /O(n)/ The number of elements.
size :: Trie a -> Int
size = fold (\_ r -> r + 1) 0

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.
prefixFind :: Key -> Trie a -> [a] 
prefixFind q n = L.map snd (prefixFindInternal split q n)

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key and include the keys 
-- in the result.
prefixFindWithKey :: Key -> Trie a -> [(Key, a)]
prefixFindWithKey = prefixFindInternal split

-- | /O(max(L,R))/ Same as 'prefixFind', but preprocesses the search key and every 
-- key in the map with @f@ before comparison.
prefixFindBy :: (Key -> Key) -> Key -> Trie a -> [a]
prefixFindBy f q n = L.map snd (prefixFindInternal (splitBy f) q n)

-- | /O(max(L,R))/ Same as 'prefixFindWithKey', but preprocesses the search key and every 
-- key in the map with @f@ before comparison.
prefixFindWithKeyBy :: (Key -> Key) -> Key -> Trie a -> [(Key, a)]
prefixFindWithKeyBy f = prefixFindInternal (splitBy f)

-- | Internal prefix find function which is used to implement every other prefix find function.
prefixFindInternal :: (Key -> Key -> (Key, Key, Key)) -> Key -> Trie a -> [(Key, a)]
prefixFindInternal f = prefixFindInternal' f []
  where
    prefixFindInternal' sf a p n | L.null pr = L.map (\(k, v) -> (a ++ k, v)) (toList n)
                                 | L.null kr = concat (L.map (prefixFindInternal' sf (a ++ (key n)) pr) (succ n))
                                 | otherwise = []
                                 where (_, pr, kr) = sf p (key n)

-- | /O(min(n,L))/ Find the value associated with a key. The function will @return@ the result in
-- the monad or @fail@ in it if the key isn't in the map.
lookup :: Monad m => Key -> Trie a -> m a
lookup q n = case lookup' q n of
             Just v -> return v
             Nothing -> fail "Trie.lookup: Key not found"

-- | Internal lookup function which is generalised for arbitrary monads above.
lookup' :: Key -> Trie a -> Maybe a
lookup' q n | L.null pr = if L.null kr then value n else Nothing
            | L.null kr = let xs = (filter isJust (L.map (lookup pr) (succ n))) in
                          if L.null xs then Nothing else head xs
            | otherwise = Nothing
            where (_, pr, kr) = split q (key n)

-- | /O(max(L,R))/ Same as 'lookup', but preprocesses the search key and every 
-- key in the map with @f@ before comparison.
lookupBy :: (Key -> Key) -> Key -> Trie a -> [(Key, a)]
lookupBy f = lookupBy' []
  where
  lookupBy' a q n | L.null pr = if L.null kr then maybe [] (\v -> [(a ++ (key n), v)]) (value n) else []
                  | L.null kr = concat (L.map (lookupBy' (a ++ (key n)) pr) (succ n))
                  | otherwise = []
                  where (_, pr, kr) = splitBy f q (key n)

-- | /O(min(n,L))/ Find the value associated with a key or return a default value if nothing 
-- was found.
findWithDefault :: a -> Key -> Trie a -> a
findWithDefault d q n = maybe d id (lookup q n)

-- | /O(n)/ Fold over all key\/value pairs in the map.
foldWithKey :: (Key -> a -> b -> b) -> b -> Trie a -> b
foldWithKey f n m = fold' [] m n
  where
  fold' ck (End k v t) r = let nk = ck ++ k in foldr (fold' nk) (f nk v r) t
  fold' ck (Seq k t) r   = let nk = ck ++ k in foldr (fold' nk) r t

-- | /O(n)/ Fold over all values in the map.
fold :: (a -> b -> b) -> b -> Trie a -> b
fold f = foldWithKey (\_ v r -> f v r)

-- | /O(n)/ Map over all key\/value pairs in the map.
mapWithKey :: (Key -> a -> b) -> Trie a -> Trie b
mapWithKey f m = map' [] m
  where
  map' ck (End k v t) = let nk = ck ++ k in End k (f nk v) (L.map (map' nk) t)
  map' ck (Seq k t)   = let nk = ck ++ k in Seq k (L.map (map' nk) t)

-- | /O(n)/ Map over all values in the map.
map :: (a -> b) -> Trie a -> Trie b
map f = mapWithKey (\_ v -> f v)

-- | /O(n)/ Convert into an ordinary map.
toMap :: Trie a -> M.Map Key a
toMap = foldWithKey M.insert M.empty

-- | /O(n)/ Convert an ordinary map into a StrMap.
fromMap :: M.Map Key a -> Trie a
fromMap = M.foldWithKey insert empty

-- | /O(n+m)/ Left-biased union of two maps. It prefers the first map when duplicate keys are 
-- encountered, i.e. ('union' == 'unionWith' 'const').
union :: Trie a -> Trie a -> Trie a
union = unionWith const

-- | /O(n+m)/ Union with a combining function.
unionWith :: (a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWith f = unionWithKey (const f)

-- | /O(n+m)/ Union with a combining function, including the key.
unionWithKey :: (Key -> a -> a -> a) -> Trie a -> Trie a -> Trie a
unionWithKey f t1 t2 | size t1 < size t2 = union' t1 t2 
                     | otherwise         = union' t2 t1
                     where
                       union' st bt = foldWithKey (\k v t -> insertWithKey f k v t) bt st

-- | /(O(n+m)/ Difference between two tries (based on keys).
difference :: Trie a -> Trie b -> Trie a
difference = differenceWith (const (const Nothing))

-- | /(O(n+m)/ Difference with a combining function. If the combining function always returns
-- 'Nothing', this is equal to proper set difference.
differenceWith :: (a -> b -> Maybe a) -> Trie a -> Trie b -> Trie a
differenceWith f = differenceWithKey (const f)

-- | /O(n+m)/ Difference with a combining function, including the key. If two equal keys are
-- encountered, the combining function is applied to the key and both values. If it returns
-- 'Nothing', the element is discarded, if it returns 'Just' a value, the element is updated
-- with the new value.
differenceWithKey :: (Key -> a -> b -> Maybe a) -> Trie a -> Trie b -> Trie a
differenceWithKey f t1 t2 = foldWithKey (\k v t -> updateWithKey (\k' v' -> f k' v' v) k t) t1 t2

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the 
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.
update :: (a -> Maybe a) -> Key -> Trie a -> Trie a
update f = updateWithKey (const f)

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the 
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.
updateWithKey :: (Key -> a -> Maybe a) -> Key -> Trie a -> Trie a
updateWithKey f k t = maybe t (\v -> maybe (delete k t) (flip (insert k) t) (f k v)) (lookup k t)

-- | /O(n)/ Returns the lengths of all keys (including keys of intermediate nodes). For 
-- debugging purposes.
lengths :: Trie a -> [Int]
lengths t = (length (key t)):(foldr (flip (++) . lengths) [] (succ t))

-- | /O(n)/ Check some invariants to detect inconsistencies.
check :: Trie a -> Bool
check (Seq [] s) = foldr check' True s
  where
  check' (Seq _ []) _   = False -- Seq node without any successor is not allowed.
  check' (Seq _ [_]) _  = False -- Seq node with just one successor is not allowed.
  check' (Seq [] _) _   = False -- Seq node with empty key is not allowed.
  check' (End [] _ _) _ = False -- End node with empty key is not allowed
  check' t r = foldr check' r (succ t)
check _ = False
