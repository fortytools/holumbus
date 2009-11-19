{-# OPTIONS -XBangPatterns #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.PrefixTree
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: not portable

  An efficient implementation of maps from strings to arbitrary values.

  Values can associated with an arbitrary byte key. Searching for keys is very fast, but
  the prefix tree probably consumes more memory than "Data.Map". The main differences are the special
  'prefixFind' functions, which can be used to perform prefix queries. The interface is
  heavily borrowed from "Data.Map" and "Data.IntMap".

  Most other function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.
  
  > import Holumbus.Data.PrefixTree (PrefixTree)
  > import qualified Holumbus.Data.PrefixTree as T

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

module Holumbus.Data.PrefixTree
{-
  (
  -- * Prefix tree types
  PrefixTree (..)
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
-}
where

import           Prelude 	hiding ( succ, lookup, map, null )

import           Control.Monad
import           Control.Parallel.Strategies


import           Data.Foldable 		( Foldable )
import qualified Data.Foldable 	as F

import           Data.Binary
import qualified Data.List 	as L
import qualified Data.Map 	as M
import           Data.Maybe
import           Data.Word

data PrefixTree v	= Empty
                        | Val	 { value' :: ! v
                                 , tree   :: ! (PrefixTree v)
                                 }
                        | Branch { sym    :: ! Sym
                                 , child  :: ! (PrefixTree v)
                                 , next   :: ! (PrefixTree v)
                                 }

                        -- the space optimisation nodes, these
                        -- will be normalized during access into
                        -- the three constructors Empty, Val and Branch

                        | Leaf   { value' :: ! v		-- a value at a leaf of the tree
                                 }
                        | Last   { sym    :: ! Sym		-- the last entry in a branch list
                                 , child  :: ! (PrefixTree v)	-- or no branch but a single child
                                 }
                        | LsSeq  { key    :: ! Key		-- a sequence of single childs
                                 , child  :: (PrefixTree v)	-- in a last node
                                 } 
                        | BrSeq  { key    :: ! Key		-- a sequence of single childs
                                 , child  :: ! (PrefixTree v)	-- in a branch node
                                 , next   :: ! (PrefixTree v)
                                 } 
                          deriving (Show, Eq, Ord)

type Sym		= Char
type Key		= [Sym]

-- ----------------------------------------

-- smart constructors

empty		:: PrefixTree v
empty		= Empty

{-# INLINE empty #-}

val		:: v -> PrefixTree v -> PrefixTree v
val v Empty	= Leaf v
val v t		= Val v t

{-# INLINE val #-}

branch		:: Sym -> PrefixTree v -> PrefixTree v -> PrefixTree v
branch k Empty         n	= n
branch k (Last   k1 c) Empty	= LsSeq [k,k1] c
branch k (LsSeq  ks c) Empty	= LsSeq (k:ks) c
branch k (Last   k1 c) n	= BrSeq [k,k1] c n
branch k (LsSeq  ks c) n	= BrSeq (k:ks) c n
branch k            c  Empty	= Last k c
branch k            c  n	= Branch k c n

{-# INLINE branch #-}

siseq		:: Key -> PrefixTree v -> PrefixTree v
siseq []   c    = c
siseq [k1] c	= Last k1 c
siseq k    c    = LsSeq  k  c

{-# INLINE siseq #-}

-- smart selectors

norm			:: PrefixTree v -> PrefixTree v
norm (Leaf v)		= Val v empty
norm (Last k c)		= Branch k c empty
norm (LsSeq [k] c)	= Branch k c empty
norm (LsSeq (k:ks) c)   = Branch k (siseq ks c) empty 
norm (BrSeq [k]    c n)	= Branch k c n
norm (BrSeq (k:ks) c n) = Branch k (siseq ks c) n 
norm t			= t

{-# INLINE norm #-}

-- ----------------------------------------

-- | /O(1)/ Is the map empty?

null 			:: PrefixTree a -> Bool
null Empty		= True
null _			= False

{-# INLINE null #-}

-- | /O(1)/ Create a map with a single element.

singleton 		:: Key -> a -> PrefixTree a
singleton k v 		= siseq k (val v empty)

{-# INLINE singleton #-}

-- | /O(1)/ Extract the value of a node (if there is one)

value 			:: Monad m => PrefixTree a -> m a
value t			= case norm t of
                          Val v _	-> return v
                          _		-> fail "PrefixTree.value: no value at this node"

{-# INLINE value #-}

-- | /O(1)/ Extract the value of a node or return a default value if no value exists.

valueWithDefault 	:: a -> PrefixTree a -> a
valueWithDefault d t	= fromMaybe d . value $ t


-- | /O(1)/ Extract the successors of a node

succ 			:: PrefixTree a -> PrefixTree a
succ t			= case norm t of
                          Val _ t	-> succ t
                          t             -> t
{-# INLINE succ #-}

-- ----------------------------------------

-- | /O(min(n,L))/ Find the value associated with a key. The function will @return@ the result in
-- the monad or @fail@ in it if the key isn't in the map.

lookup 			:: Monad m => Key -> PrefixTree a -> m a
lookup q n 		= case lookup' q n of
                          Just v  -> return v
                          Nothing -> fail "PrefixTree.lookup: Key not found"

{-# INLINE lookup #-}

-- | /O(min(n,L))/ Is the key a member of the map?

member 			:: Key -> PrefixTree a -> Bool
member k m 		= maybe False (const True) $ lookup k m

{-# INLINE member #-}

-- | /O(min(n,L))/ Find the value at a key. Calls error when the element can not be found.

(!) 			:: PrefixTree a -> Key -> a
(!) m k 		= fromMaybe (error "PrefixTree.! : element not in the map")
			  . lookup k $ m

-- | /O(min(n,L))/ Insert a new key and value into the map. If the key is already present in
-- the map, the associated value will be replaced with the new value.

insert 				:: Key -> a -> PrefixTree a -> PrefixTree a
insert 				= insertWith const

{-# INLINE insert #-}

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f new_value old_value@ will be inserted.

insertWith 			:: (a -> a -> a) -> Key -> a -> PrefixTree a -> PrefixTree a
insertWith f			= flip $ insert' f

{-# INLINE insertWith #-}

-- | /O(min(n,L))/ Insert with a combining function. If the key is already present in the map,
-- the value of @f key new_value old_value@ will be inserted.

insertWithKey 			:: (Key -> a -> a -> a) -> Key -> a -> PrefixTree a -> PrefixTree a
insertWithKey f k	 	= insertWith (f k) k


{-# INLINE insertWithKey #-}

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the 
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.

updateWith			:: (a -> Maybe a) -> Key -> PrefixTree a -> PrefixTree a
updateWith			= update'

{-# INLINE updateWith #-}

-- | /O(min(n,L))/ Delete an element from the map. If no element exists for the key, the map 
-- remains unchanged.

delete 				:: Key -> PrefixTree a -> PrefixTree a
delete 				= update' (const Nothing)

{-# INLINE delete #-}

-- ----------------------------------------
-- Internal lookup function which is generalised for arbitrary monads above.

lookup' 		:: Key -> PrefixTree a -> Maybe a
lookup' []     t	= value t
lookup' (k:ks) t
    | null t		= Nothing
    | otherwise		= lookup' ks child
    where
    child		= lookupBr k (succ t)

    lookupBr 		:: Sym -> PrefixTree a -> PrefixTree a
    lookupBr s t	= case norm t of
                          Branch s1 ch nx -> if s < s1
                                             then empty
                                             else if s == s1
                                                  then ch
                                                  else lookupBr s nx
                          _               -> empty

-- ----------------------------------------

insert' 			:: (a -> a -> a) -> a -> Key -> PrefixTree a -> PrefixTree a
insert' f v k			= ins k . norm
    where
    ins'                	= insert' f v

    ins k (Branch c' s' n')
	= case k of
	  []			-> val v (branch c' s' n')
          (c : k1)
              | c <  c'		-> branch c (singleton k1 v) (branch c' s' n')
              | c == c'		-> branch c (ins' k1 s')                   n'
              | otherwise	-> branch c'         s'            (ins' k n')

    ins k  Empty        	= singleton k     v

    ins k (Val v' t')
	= case k of
          []			-> val (f v v') t'
          _			-> val      v'  (ins' k t')

-- ----------------------------------------

update'				:: (a -> Maybe a) -> Key -> PrefixTree a -> PrefixTree a
update' f k			= upd k . norm
    where
    upd'			= update' f

    upd k (Branch c' s' n')
	= case k of
	  []			-> branch c' s' n'
          (c : k1)
              | c <  c'		-> branch c' s' n'
              | c == c'		-> branch c (upd' k1 s')            n'
              | otherwise	-> branch c'         s'     (upd' k n')

    upd k Empty			= empty

    upd k (Val v' t')
        = case k of
          []			-> maybe t' (flip val t') $ f v'
          _			-> val v' (upd' k t')

-- ----------------------------------------

union' 				:: (a -> a -> a) -> PrefixTree a -> PrefixTree a -> PrefixTree a
union' f t1 t2			= uni (norm t1) (norm t2)
    where
    uni' t1' t2'		= union' f (norm t1') (norm t2')

    uni     Empty                Empty		= empty
    uni     Empty               (Val v2 t2)	= val v2 t2
    uni     Empty               (Branch c2 s2 n2)
						= branch c2 s2 n2

    uni    (Val v1 t1)           Empty		= val    v1     t1
    uni    (Val v1 t1)          (Val v2 t2)	= val (f v1 v2) (uni' t1 t2)
    uni    (Val v1 t1)       t2@(Branch _ _ _)	= val    v1     (uni' t1 t2)

    uni    (Branch c1 s1 n1)     Empty		= branch c1 s1 n1
    uni t1@(Branch c1 s1 n1)    (Val v2 t2)	= val v2 (uni' t1 t2) 
    uni t1@(Branch c1 s1 n1) t2@(Branch c2 s2 n2)
        | c1 <  c2				= branch c1       s1     (uni' n1 t2)
        | c1 >  c2				= branch c2          s2  (uni' t1 n2)
        | otherwise				= branch c1 (uni' s1 s2) (uni' n1 n2)

-- ----------------------------------------

-- | /O(n)/ Map a function over all values in the prefix tree.

map 				:: (a -> b) -> PrefixTree a -> PrefixTree b
map f 				= mapWithKey (const f)


mapWithKey 			:: (Key -> a -> b) -> PrefixTree a -> PrefixTree b
mapWithKey f 			= map' f id


map'				:: (Key -> a -> b) -> (Key -> Key) -> PrefixTree a -> PrefixTree b
map' f k (Empty)		= Empty
map' f k (Val v t)		= Val    (f (k []) v)    (map' f k t)
map' f k (Branch c s n)         = Branch c (map' f ((c :) . k) s) (map' f k n)
map' f k (Leaf v)		= Leaf   (f (k []) v)
map' f k (Last c s)		= Last c  (map' f ((c :)   . k) s)
map' f k (LsSeq cs s)		= LsSeq  cs (map' f ((cs ++) . k) s)
map' f k (BrSeq cs s n)         = BrSeq  cs (map' f ((cs ++) . k) s) (map' f k n)

-- ----------------------------------------

-- | /O(n)/ Fold over all key\/value pairs in the map.

foldWithKey 			:: (Key -> a -> b -> b) -> b -> PrefixTree a -> b
foldWithKey f e 		= fold' f e id

{-# INLINE foldWithKey #-}

-- | /O(n)/ Fold over all values in the map.

fold :: (a -> b -> b) -> b -> PrefixTree a -> b
fold f = foldWithKey $ const f

{-# INLINE fold #-}

foldTopDown			:: (Key -> a -> b -> b) -> b -> (Key -> Key) -> PrefixTree a -> b
foldTopDown f r k		= fo k . norm
    where
    fo k (Branch c' s' n')	= let r' = foldTopDown f r ((c' :) . k) s' in foldTopDown f r' k n'
    fo k (Empty)		= r
    fo k (Val v' t')		= let r' = f (k []) v' r                   in foldTopDown f r' k t'


fold'				:: (Key -> a -> b -> b) -> b -> (Key -> Key) -> PrefixTree a -> b
fold' f r k			= fo k . norm
    where
    fo k (Branch c' s' n')	= let r' = fold' f r k n' in fold' f r' ((c' :) . k) s'
    fo k (Empty)		= r
    fo k (Val v' t')		= let r' = fold' f r k t' in f (k []) v' r'

-- | /O(n)/ Convert into an ordinary map.

toMap 				:: PrefixTree a -> M.Map Key a
toMap 				= foldWithKey M.insert M.empty

-- | /O(n)/ Convert an ordinary map into a Prefix tree

fromMap 			:: M.Map Key a -> PrefixTree a
fromMap 			= M.foldWithKey insert empty

-- | /O(n)/ Returns all elements as list of key value pairs,

toList 				:: PrefixTree a -> [(Key, a)]
toList 				= foldWithKey (\k v r -> (k, v) : r) []

-- | /O(n)/ Creates a trie from a list of key\/value pairs.
fromList 			:: [(Key, a)] -> PrefixTree a
fromList 			= L.foldl' (\p (k, v) -> insert k v p) empty

-- | /O(n)/ The number of elements.
size 				:: PrefixTree a -> Int
size 				= fold (const (+1)) 0

-- | /O(n)/ Returns all values.
elems 				:: PrefixTree a -> [a]
elems   			= fold (:) []

-- | /O(n)/ Returns all values.
keys 				:: PrefixTree a -> [Key]
keys	   			= foldWithKey (\ k v r -> k : r) []

-- ----------------------------------------

instance Functor PrefixTree where
  fmap = map

instance Foldable PrefixTree where
  foldr = fold

{- for debugging not yet enabled

instance Show a => Show (PrefixTree a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-}

instance Read a => Read (PrefixTree a) where
  readsPrec p = readParen (p > 10) $
    \ r -> do
	   ("fromList",s) <- lex r
	   (xs,t) <- reads s
	   return (fromList xs,t)

instance NFData a => NFData (PrefixTree a) where
    rnf (Empty)		= ()
    rnf (Val v t)	= rnf v `seq` rnf t
    rnf (Branch _c s n)	= rnf s `seq` rnf n
    rnf (Leaf v)	= rnf v
    rnf (Last _c s)	= rnf s
    rnf (LsSeq ks s)	= rnf ks `seq` rnf s
    rnf (BrSeq ks s n)	= rnf ks `seq` rnf s `seq` rnf n

-- Provide native binary serialization (not via to-/fromList).

instance (Binary a) => Binary (PrefixTree a) where
    put (Empty)		= put (0::Word8)
    put (Val v t)	= put (1::Word8) >> put v >> put t
    put (Branch c s n)  = put (2::Word8) >> put c >> put s >> put n
    put (Leaf v)	= put (3::Word8) >> put v
    put (Last c s)	= put (4::Word8) >> put c >> put s
    put (LsSeq k s)	= put (5::Word8) >> put k >> put s
    put (BrSeq k s n) 	= put (6::Word8) >> put k >> put s >> put n

    get = do
	  !tag <- getWord8
	  case tag of
		   0 -> return Empty
		   1 -> do
			!v <- get
			!t <- get
			return $! Val v t
		   2 -> do
			!c <- get
			!s <- get
			!n <- get
			return $! Branch c s n
		   3 -> do
			!v <- get
			return $! Leaf v
		   4 -> do
			!c <- get
			!s <- get
			return $! Last c s
		   5 -> do
			!k <- get
			!s <- get
			return $! LsSeq k s
		   6 -> do
			!k <- get
			!s <- get
			!n <- get
			return $! BrSeq k s n
		   _ -> fail "PrefixTree.get: error while decoding PrefixTree"

{-

-- | /O(1)/ Extract the key of a node
key :: PrefixTree a -> Key
key (End k _ _) = k
key (Seq k _)   = k

-- | /O(1)/ Extract the successors of a node
succ :: PrefixTree a -> [PrefixTree a]
succ (End _ _ t) = t
succ (Seq _ t)   = t

-- | /O(1)/ Sets the key of a node.
setKey :: Key -> PrefixTree a -> PrefixTree a
setKey k (End _ v t) = End k v t
setKey k (Seq _ t)   = Seq k t

-- | /O(1)/ Sets the successors of a node.
setSucc :: [PrefixTree a] -> PrefixTree a -> PrefixTree a
setSucc t (End k v _) = End k v t
setSucc t (Seq k _)   = Seq k t

-- | Merge a node with its successor if only one successor is left.
mergeNode :: PrefixTree a -> [PrefixTree a] -> PrefixTree a
mergeNode (End k v _) t = End k v t
mergeNode (Seq k _) [t] = if not (L.null k) then setKey (k ++ (key t)) t else Seq k [t]
mergeNode (Seq k _) t   = Seq k t

-- | Delete a node by either merging it with its successors or removing it completely.
deleteNode :: PrefixTree a -> Maybe (PrefixTree a)
deleteNode (End _ _ [])  = Nothing
deleteNode (End k _ [t]) = Just (setKey (k ++ key t) t)
deleteNode (End k _ t)   = Just (Seq k t)
deleteNode n             = Just n

-- | Internal support function for insert which searches the correct successor to insert into
-- within a list of nodes (the successors of the current node, see call in insert' above).
insertSub :: (Key -> a -> a -> a) -> Key -> a -> Key -> [PrefixTree a] -> [PrefixTree a]
insertSub f k v o t = insertSub' f k v t []
  where
    insertSub' :: (Key -> a -> a -> a) -> Key -> a -> [PrefixTree a] -> [PrefixTree a] -> [PrefixTree a]
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
elems :: PrefixTree a -> [a]
elems t   = L.map snd (toList t)

-- | /O(n)/ Creates a trie from a list of key\/value pairs.
fromList :: [(Key, a)] -> PrefixTree a
fromList xs = L.foldl' (\p (k, v) -> insert k v p) empty xs

-- | /O(n)/ Returns all elements as list of key value pairs,
toList :: PrefixTree a -> [(Key, a)]
toList = foldWithKey (\k v r -> (k, v):r) []

-- | /O(n)/ The number of elements.
size :: PrefixTree a -> Int
size = fold (\_ r -> r + 1) 0

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.
prefixFind :: Key -> PrefixTree a -> [a] 
prefixFind q n = L.map snd (prefixFindInternal split q n)

-- | /O(max(L,R))/ Find all values where the string is a prefix of the key and include the keys 
-- in the result.
prefixFindWithKey :: Key -> PrefixTree a -> [(Key, a)]
prefixFindWithKey = prefixFindInternal split

-- | /O(max(L,R))/ Same as 'prefixFind', but preprocesses the search key and every 
-- key in the map with @f@ before comparison.
prefixFindBy :: (Key -> Key) -> Key -> PrefixTree a -> [a]
prefixFindBy f q n = L.map snd (prefixFindInternal (splitBy f) q n)

-- | /O(max(L,R))/ Same as 'prefixFindWithKey', but preprocesses the search key and every 
-- key in the map with @f@ before comparison.
prefixFindWithKeyBy :: (Key -> Key) -> Key -> PrefixTree a -> [(Key, a)]
prefixFindWithKeyBy f = prefixFindInternal (splitBy f)

-- | Internal prefix find function which is used to implement every other prefix find function.
prefixFindInternal :: (Key -> Key -> (Key, Key, Key)) -> Key -> PrefixTree a -> [(Key, a)]
prefixFindInternal f = prefixFindInternal' f []
  where
    prefixFindInternal' sf a p n | L.null pr = L.map (\(k, v) -> (a ++ k, v)) (toList n)
                                 | L.null kr = concat (L.map (prefixFindInternal' sf (a ++ (key n)) pr) (succ n))
                                 | otherwise = []
                                 where (_, pr, kr) = sf p (key n)

-- | /O(min(n,L))/ Find the value associated with a key. The function will @return@ the result in
-- the monad or @fail@ in it if the key isn't in the map.
lookup :: Monad m => Key -> PrefixTree a -> m a
lookup q n = case lookup' q n of
             Just v -> return v
             Nothing -> fail "PrefixTree.lookup: Key not found"

-- | Internal lookup function which is generalised for arbitrary monads above.
lookup' :: Key -> PrefixTree a -> Maybe a
lookup' q n | L.null pr = if L.null kr then value n else Nothing
            | L.null kr = let xs = (filter isJust (L.map (lookup' pr) (succ n))) in
                          if L.null xs then Nothing else head xs
            | otherwise = Nothing
            where (_, pr, kr) = split q (key n)

-- | /O(max(L,R))/ Same as 'lookup', but preprocesses the search key and every 
-- key in the map with @f@ before comparison.
lookupBy :: (Key -> Key) -> Key -> PrefixTree a -> [(Key, a)]
lookupBy f = lookupBy' []
  where
  lookupBy' a q n | L.null pr = if L.null kr then maybe [] (\v -> [(a ++ (key n), v)]) (value n) else []
                  | L.null kr = concat (L.map (lookupBy' (a ++ (key n)) pr) (succ n))
                  | otherwise = []
                  where (_, pr, kr) = splitBy f q (key n)

-- | /O(min(n,L))/ Find the value associated with a key or return a default value if nothing 
-- was found.
findWithDefault :: a -> Key -> PrefixTree a -> a
findWithDefault d q n = maybe d id (lookup q n)

-- | /O(n+m)/ Left-biased union of two maps. It prefers the first map when duplicate keys are 
-- encountered, i.e. ('union' == 'unionWith' 'const').
union :: PrefixTree a -> PrefixTree a -> PrefixTree a
union = unionWith const

-- | /O(n+m)/ Union with a combining function.
unionWith :: (a -> a -> a) -> PrefixTree a -> PrefixTree a -> PrefixTree a
unionWith f = unionWithKey (const f)

-- | /O(n+m)/ Union with a combining function, including the key.
unionWithKey :: (Key -> a -> a -> a) -> PrefixTree a -> PrefixTree a -> PrefixTree a
unionWithKey f t1 t2 | size t1 < size t2 = union' t1 t2 
                     | otherwise         = union' t2 t1
                     where
                       union' st bt = foldWithKey (\k v t -> insertWithKey f k v t) bt st

-- | /(O(n+m)/ Difference between two tries (based on keys).
difference :: PrefixTree a -> PrefixTree b -> PrefixTree a
difference = differenceWith (const (const Nothing))

-- | /(O(n+m)/ Difference with a combining function. If the combining function always returns
-- 'Nothing', this is equal to proper set difference.
differenceWith :: (a -> b -> Maybe a) -> PrefixTree a -> PrefixTree b -> PrefixTree a
differenceWith f = differenceWithKey (const f)

-- | /O(n+m)/ Difference with a combining function, including the key. If two equal keys are
-- encountered, the combining function is applied to the key and both values. If it returns
-- 'Nothing', the element is discarded, if it returns 'Just' a value, the element is updated
-- with the new value.
differenceWithKey :: (Key -> a -> b -> Maybe a) -> PrefixTree a -> PrefixTree b -> PrefixTree a
differenceWithKey f t1 t2 = foldWithKey (\k v t -> updateWithKey (\k' v' -> f k' v' v) k t) t1 t2

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the 
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.
update :: (a -> Maybe a) -> Key -> PrefixTree a -> PrefixTree a
update f = updateWithKey (const f)

-- | /O(min(n,L))/ Updates a value at a given key (if that key is in the trie) or deletes the 
-- element if the result of the updating function is 'Nothing'. If the key is not found, the trie
-- is returned unchanged.
updateWithKey :: (Key -> a -> Maybe a) -> Key -> PrefixTree a -> PrefixTree a
updateWithKey f k t = maybe t (\v -> maybe (delete k t) (flip (insert k) t) (f k v)) (lookup k t)

-- | /O(n)/ Returns the lengths of all keys (including keys of intermediate nodes). For 
-- debugging purposes.
lengths :: PrefixTree a -> [Int]
lengths t = (length (key t)):(foldr (flip (++) . lengths) [] (succ t))

-- | /O(n)/ Check some invariants to detect inconsistencies.
check :: PrefixTree a -> Bool
check (Seq [] s) = foldr check' True s
  where
  check' (Seq _ []) _   = False -- Seq node without any successor is not allowed.
  check' (Seq _ [_]) _  = False -- Seq node with just one successor is not allowed.
  check' (Seq [] _) _   = False -- Seq node with empty key is not allowed.
  check' (End [] _ _) _ = False -- End node with empty key is not allowed
  check' t r = foldr check' r (succ t)
check _ = False

-}