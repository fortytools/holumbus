{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Applicative
import           Control.DeepSeq

import           Data.Binary         (Binary (..), getWord8)
import qualified Data.Foldable       as F
import qualified Data.List           as L
import           Data.Traversable    (Traversable (..))
import           Data.Word           (Word8)
import           Prelude             hiding (foldl, foldr, lookup, map)

moduleName :: String
moduleName = "Data.IntMap.BinTree.Strict"

error' :: String -> a
error' = error . ((moduleName ++": ") ++)

-- ----------------------------------------

type Key = Int

data Tree v = Empty
            | Node !Key !v !(Tree v) !(Tree v)
            | Lt   !Key !v !(Tree v)            -- Lt, Gt and Leaf are special cases
            | Gt   !Key !v           !(Tree v)  -- of Node to get rid of the empty trees
            | Leaf !Key !v                      -- Empty occurs only as root, never as subtree
              deriving (Show)                   -- This saves 20% of space, average size of
                                                -- objects is 4 instead of 5 words

-- ----------------------------------------
--
-- instances for NFData and Binary

instance NFData v => NFData (Tree v) where
    rnf (Node _k v l r) = rnf v `seq` rnf l `seq` rnf r
    rnf (Lt   _k v l  ) = rnf v `seq` rnf l
    rnf (Gt   _k v   r) = rnf v `seq`             rnf r
    rnf (Leaf _k v    ) = rnf v
    rnf Empty           = ()

instance Binary v => Binary (Tree v) where
    put (Empty       ) = put (0::Word8)
    put (Node k v l r) = put (1::Word8) >> put k >> put v >> put l >> put r
    put (Lt   k v l  ) = put (2::Word8) >> put k >> put v >> put l
    put (Gt   k v   r) = put (3::Word8) >> put k >> put v          >> put r
    put (Leaf k v    ) = put (4::Word8) >> put k >> put v

    get = do tag <- getWord8
             case tag of
               0 -> return Empty
               1 -> Node <$> get <*> get <*> get <*> get
               2 -> Lt   <$> get <*> get <*> get
               3 -> Gt   <$> get <*> get         <*> get
               4 -> Leaf <$> get <*> get
               _ -> error' "error in \"get\" while decoding BinTree"

instance Functor Tree where
    fmap = map

instance F.Foldable Tree where
    foldr  = foldr
    foldl  = foldl
    foldr' = foldr'
    foldl' = foldl'

instance Traversable Tree where
    traverse f (Node k v l r) = Node k <$> f v <*> traverse f l <*> traverse f r
    traverse f (Lt   k v l  ) = Lt   k <$> f v <*> traverse f l
    traverse f (Gt   k v   r) = Gt   k <$> f v                  <*> traverse f r
    traverse f (Leaf k v    ) = Leaf k <$> f v
    traverse _ (Empty       ) = pure Empty

-- ----------------------------------------
--
-- the smart constructor generating Lt, Gt and Leaf nodes

mkNode :: Key -> v -> Tree v -> Tree v -> Tree v
mkNode k v Empty Empty = Leaf k v
mkNode k v Empty r     = Gt   k v   r
mkNode k v l     Empty = Lt   k v l
mkNode k v l     r     = Node k v l r

{-# INLINE mkNode #-}


-- the smart destructor for normalization of Lt, Gt and Leaf nodes

unNode :: Tree v -> (Key, v, Tree v, Tree v)
unNode (Node k v l r) = (k, v, l,     r    )
unNode (Lt   k v l  ) = (k, v, l,     Empty)
unNode (Gt   k v   r) = (k, v, Empty, r    )
unNode (Leaf k v    ) = (k, v, Empty, Empty)
unNode  Empty         = error' "\"unNode\" with empty tree"

{-# INLINE unNode #-}

-- ----------------------------------------
--
-- the work horses

split' :: Key -> Tree v -> (Maybe v, Tree v, Tree v)
split' _ Empty
    = (Nothing, Empty, Empty)

split' k t
    = case compare k k' of
        LT -> let (v', l', r') = split' k l
              in  (v', l', mkNode k' v r' r)

        EQ -> (Just v, l, r)

        GT -> let (v', l', r') = split' k r
              in  (v', mkNode k' v l l', r')
      where
        (k', v, l, r) = unNode t

join' :: Maybe (Key, v) -> Tree v -> Tree v -> Tree v
join' (Just (k, v)) t1 t2
    = mkNode k v t1 t2

join' Nothing t1 t2
    = case minViewWithKey t2 of                 -- get smallest key in t2
        Nothing          -> t1                  -- and return t1
        Just ((k, v), r) -> mkNode k v t1 r     -- that key as new root

{- disabled due to unbalancing of result tree

join' Nothing t1 t2
    = go t1
      where
        go Empty = t2                           -- insert whole t2 at the rightmost node in t1
        go t     = mkNode k v l (go r)          -- balancing ???
                   where
                     (k, v, l, r) = unNode t
-- -}

{-# INLINE join' #-}

-- ----------------------------------------
{-
-- lookup, insert and delete with split' and join'
-- insert and delete change the root
-- and can lead to stronger unbalancing than trad. insert and delete

lookup :: Key -> Tree v -> Maybe v
lookup k t
    = v
      where
        (v, _l, _r) = split' k t

insertWith :: (v -> v -> v) -> Key -> v -> Tree v -> Tree v
insertWith f k v t
    = join' (Just (k, f' v')) l r
      where
        (v', l, r)    = split' k t
        f' Nothing    =       v
        f' (Just v'') = f v'' v

delete :: Key -> Tree v -> Tree v
delete k t
    = join' Nothing l r
      where
        (_, l, r) = split' k t

{-# INLINE lookup     #-}
{-# INLINE insertWith #-}
{-# INLINE delete     #-}

-- -}
-- ----------------------------------------
-- {-
-- traditional lookup, insert and remove

lookup :: Key -> Tree v -> Maybe v
lookup k
    = go
      where
        go Empty = Nothing
        go t     = case compare k k' of
                     LT -> go l
                     EQ -> Just v
                     GT -> go r
                   where
                     (k', v, l, r) = unNode t

insertWith :: (v -> v -> v) -> Key -> v -> Tree v -> Tree v
insertWith f k v
    = ins
    where
      ins Empty = mkNode k v Empty Empty
      ins t     = case compare k k' of
                    LT -> mkNode k' v'      (ins l)      r
                    EQ -> mkNode k (f v v')      l       r
                    GT -> mkNode k' v'           l  (ins r)
                  where
                    (k', v', l, r) = unNode t

delete :: Key -> Tree v -> Tree v
delete k
    = del
    where
      del Empty = Empty
      del t     = case compare k k' of
                    LT -> mkNode k' v' (del l) r
                    EQ -> case minViewWithKey r of
                            Nothing               -> Empty
                            Just ((k'', v''), r') -> mkNode k'' v'' l r'
                    GT -> mkNode k' v' l (del r)
                  where
                    (k', v', l, r) = unNode t

{-# INLINE lookup          #-}
{-# INLINE insertWith      #-}
{-# INLINE delete          #-}

-- -}
-- ----------------------------------------
--
-- derived lookup and insert functions

find :: Key -> Tree v -> v
find k = maybe notThere id . lookup k
    where
      notThere = error' ( "error in find: key "
                         ++ show k
                         ++ " is not an element of the map"
                       )

findWithDefault :: v -> Key -> Tree v -> v
findWithDefault v k = maybe v id . lookup k

member :: Key -> Tree v -> Bool
member k = maybe False (const True) . lookup k

notMember :: Key -> Tree v -> Bool
notMember k = maybe True (const False) . lookup k


insert :: Key -> v -> Tree v -> Tree v
insert = insertWith const


{-# INLINE find            #-}
{-# INLINE findWithDefault #-}
{-# INLINE member          #-}
{-# INLINE notMember       #-}
{-# INLINE insert          #-}

-- ----------------------------------------

union :: Tree v -> Tree v -> Tree v
union = unionWith const

unionWith :: (v -> v -> v) -> Tree v -> Tree v -> Tree v
unionWith op
    = unionWithKey (const op)

unionWithKey :: (Key -> v -> v -> v) -> Tree v -> Tree v -> Tree v
unionWithKey _  x1 Empty
    = x1
unionWithKey f x1 x2
    = uni x1 x2
    where
      uni Empty t2 = t2
      uni t1    t2 = join' (Just (k, v')) (uni l l') (uni r r')
                     where
                       (k, v, l, r) = unNode t1
                       (m', l', r') = split' k t2
                       v'           = maybe v (f k v) m'

{-# INLINE union        #-}
{-# INLINE unionWith    #-}
{-# INLINE unionWithKey #-}

-- ----------------------------------------

difference :: Tree a -> Tree b -> Tree a
difference = differenceWith (const (const Nothing))

differenceWith :: (a -> b -> Maybe a) -> Tree a -> Tree b -> Tree a
differenceWith op
    = differenceWithKey (const op)

differenceWithKey :: (Key -> a -> b -> Maybe a) -> Tree a -> Tree b -> Tree a
differenceWithKey _ x1 Empty
    = x1

differenceWithKey f x1 x2
    = diff x1 x2
    where
      diff Empty _  = Empty
      diff t1    t2 = join' v' (diff l l') (diff r r')
                      where
                        (k, v, l, r) = unNode t1
                        (m', l', r') = split' k t2
                        v'           = case m' of
                                          Nothing -> Just (k, v)
                                          Just x  -> case f k v x of
                                                       Nothing -> Nothing
                                                       Just y  -> Just (k, y)

{-# INLINE difference        #-}
{-# INLINE differenceWith    #-}
{-# INLINE differenceWithKey #-}

-- ----------------------------------------

intersection :: Tree a -> Tree b -> Tree a
intersection = intersectionWith const

intersectionWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
intersectionWith op
    = intersectionWithKey (const op)

intersectionWithKey :: (Key -> a -> b -> c) -> Tree a -> Tree b -> Tree c
intersectionWithKey _  _ Empty
    = Empty
intersectionWithKey f x1 x2
    = intersect x1 x2
    where
      intersect Empty _  = Empty
      intersect t1    t2 = join' kv' (intersect l l') (intersect r r')
                           where
                             (k, v, l, r) = unNode t1
                             (m', l', r') = split' k t2
                             kv'          = (\ y -> (k, f k v y)) <$> m'

{-# INLINE intersection         #-}
{-# INLINE intersectionWith     #-}
{-# INLINE intersectionWithKey  #-}

-- ----------------------------------------
--
-- maps

map :: (a -> b) -> Tree a -> Tree b
map f = mapWithKey (const f)

mapWithKey :: (Key -> a -> b) -> Tree a -> Tree b
mapWithKey f
    = mp
    where
      mp Empty = Empty
      mp t     = mkNode k (f k v) (mp l) (mp r)
                 where
                   (k, v, l, r) = unNode t

{-# INLINE map                  #-}
{-# INLINE mapWithKey           #-}

-- ----------------------------------------
-- filter

filter :: (a -> Bool) -> Tree a -> Tree a
filter p = filterWithKey (\ _k v -> p v)

filterWithKey :: (Key -> a -> Bool) -> Tree a -> Tree a
filterWithKey p
    = go
    where
      go Empty = Empty
      go t     = join' res (go l) (go r)
                 where
                   (k, v, l, r)    = unNode t
                   res | p k v     = Just (k, v)
                       | otherwise = Nothing

{-# INLINE filter               #-}
{-# INLINE filterWithKey        #-}

-- ----------------------------------------
--
-- foldr's

foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr op = foldrWithKey (const op)

foldr' :: (a -> b -> b) -> b -> Tree a -> b
foldr' op = foldrWithKey' (const op)

foldrWithKey :: (Key -> a -> b -> b) -> b -> Tree a -> b
foldrWithKey f
    = fold
    where
      fold acc Empty = acc
      fold acc t     = fold (f k v (fold acc r)) l
                       where
                         (k, v, l, r) = unNode t

foldrWithKey' :: (Key -> a -> b -> b) -> b -> Tree a -> b
foldrWithKey' f
    = fold
    where
      fold !acc Empty = acc
      fold !acc t     = fold (f k v (fold acc r)) l
                        where
                          (k, v, l, r) = unNode t

-- ----------------------------------------
--
-- foldl's

foldl :: (b -> a -> b) -> b -> Tree a -> b
foldl op = foldlWithKey (\ x _k v -> op x v)

foldl' :: (b -> a -> b) -> b -> Tree a -> b
foldl' op = foldlWithKey (\ x _k v -> op x v)

foldlWithKey :: (b -> Key -> a -> b) -> b -> Tree a -> b
foldlWithKey f
    = fold
    where
      fold acc Empty = acc
      fold acc t     = fold (f (fold acc l) k v) r
                       where
                         (k, v, l, r) = unNode t

foldlWithKey' :: (b -> Key -> a -> b) -> b -> Tree a -> b
foldlWithKey' f
    = fold
    where
      fold !acc Empty = acc
      fold !acc t     = fold (f (fold acc l) k v) r
                        where
                          (k, v, l, r) = unNode t

{-# INLINE foldr        #-}
{-# INLINE foldrWithKey #-}
{-# INLINE foldl        #-}
{-# INLINE foldlWithKey #-}

-- ----------------------------------------

fromList :: [(Key, v)] -> Tree v
fromList = L.foldl' (\ acc (k, v) -> insert k v acc) Empty

-- ----------------------------------------
--
-- conversions to lists

toAscList :: Tree v -> [(Key, v)]
toAscList = foldrWithKey (\ k v res -> (k, v) : res) []

toList :: Tree v -> [(Key, v)]
toList = toAscList

assocs :: Tree v -> [(Key, v)]
assocs = toAscList

elems :: Tree v -> [v]
elems = foldr (:) []

keys :: Tree v -> [Key]
keys = foldrWithKey (\ k _v r -> k : r) []

{-# INLINE toAscList #-}
{-# INLINE toList #-}
{-# INLINE assocs #-}
{-# INLINE elems #-}
{-# INLINE keys  #-}

-- ----------------------------------------
--
-- min/max

minView :: Tree v -> Maybe (v, Tree v)
minView t = first snd <$> minViewWithKey t

minViewWithKey :: Tree v -> Maybe ((Key, v), Tree v)
minViewWithKey Empty = Nothing
minViewWithKey x     = go x
    where
      go t = case l of
               Empty -> Just ((k, v), r)
               _     -> go l
          where (k, v, l, r) = unNode t

maxView :: Tree v -> Maybe (v, Tree v)
maxView t = first snd <$> maxViewWithKey t

maxViewWithKey :: Tree v -> Maybe ((Key, v), Tree v)
maxViewWithKey Empty = Nothing
maxViewWithKey x     = go x
    where
      go t = case r of
               Empty -> Just ((k, v), l)
               _     -> go r
          where (k, v, l, r) = unNode t

{-# INLINE minView        #-}
{-# INLINE maxView        #-}
{-# INLINE minViewWithKey #-}
{-# INLINE maxViewWithKey #-}

first :: (a -> c) -> (a, b) -> (c, b)
first f (x,y) = (f x,y)

{-# INLINE first #-}

-- ----------------------------------------


main :: IO ()
main = return ()

fromList' :: [Key] -> Tree String
fromList' = fromList . L.map (\ x -> (x, show x))
fromList'' :: [Key] -> Tree String
fromList'' = fromList . L.map (\ x -> (x, show $ x+1))

s1 :: Tree String
s1 = fromList' [2,4..10]
s2 :: Tree String
s2 = fromList' [1,3..10]
s3 :: Tree String
s3 = fromList'' [0,3..10]

