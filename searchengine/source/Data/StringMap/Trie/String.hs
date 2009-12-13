-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Trie.String
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental
  Portability: portable

  An efficient implementation of maps from string keys to arbitrary values, 
  used for the Holumbus indexes.

  The implementation is derived from the StrMap source,
  but the StrMap type is implemented as a newtype,
  and it is an instance of the tye class StringMap,
  which unifies the interface of the various implementations of
  a map from String to values.
-}

-- ----------------------------------------------------------------------------

module Data.StringMap.Trie.String
  (
  -- * Map type
  StringTrie

  -- * Debug
  , lengths
  , check
  , space
  , keyChars
  )
where

import           Prelude 			hiding (lookup, map, null)

import           Control.Arrow
import           Control.Parallel.Strategies

import           Codec.Binary.UTF8.String

import           Data.Binary          		(Binary, get, put)
import           Data.Char
import           Data.Foldable       		(Foldable, foldr)
import qualified Data.Map 			as M
import qualified Data.List 			as L

import           Data.StringMap.Class
import           Data.StringMap.Trie.Word8	(Trie, Key)
import qualified Data.StringMap.Trie.Word8	as T

-- ----------------------------------------

-- | A map from strings to values a, implemeted as a wrapped trie.

newtype StringTrie a 	= ST { trie :: Trie a }

-- ----------------------------------------
--
-- constructors

empty 			:: StringTrie a
empty 			= ST $ T.empty

singleton 		:: String -> a -> StringTrie a
singleton k 		= ST . T.singleton (encode k)

fromList 		:: [(String, a)] -> StringTrie a
fromList 		= ST . T.fromList . (L.map (first encode))

fromMap 		:: M.Map String a -> StringTrie a
fromMap 		= ST . T.fromMap . (M.mapKeys encode)

-- ----------------------------------------
--
-- helper

liftST			:: (Trie a -> Trie b) -> (StringTrie a -> StringTrie b)
liftST f                = ST . f . trie

liftST2			:: (Trie a -> Trie b -> Trie c) -> (StringTrie a -> StringTrie b -> StringTrie c)
liftST2 f m1            = ST . f (trie m1) . trie

lower 			:: Key -> Key
lower 			= encode . (L.map toLower) . decode

-- ----------------------------------------
--
-- debug

lengths 		:: StringTrie a -> [Int]
lengths 		= T.lengths . trie

check 			:: StringTrie a -> Bool
check 			= T.check . trie

space			:: StringTrie a -> Int
space			= T.space . trie

keyChars		:: StringTrie a -> Int
keyChars		= T.keyChars . trie

-- ----------------------------------------
--
-- constructors

instance StringMapConstructors StringTrie where
    empty 			= ST $ T.empty
    singleton k 		= ST . T.singleton (encode k)
    fromList 			= ST . T.fromList . (L.map (first encode))
    fromMap 			= ST . T.fromMap . (M.mapKeys encode)

-- ----------------------------------------

instance StringMap StringTrie where
    (!) m k 			= (T.!) (trie m) (encode k)
    null 			= T.null . trie
    size 			= T.size . trie
    member k 			= T.member (encode k) . trie
    insertWithKey f k v 	= liftST $ T.insertWithKey (f . decode) (encode k) v
    insertWith f k v 		= liftST $ T.insertWith f (encode k) v
    insert k v	 		= liftST $ T.insert (encode k) v
    delete k 			= liftST $ T.delete (encode k)
    update f k 			= liftST $ T.update f (encode k)
    updateWithKey f k 		= liftST $ T.updateWithKey (f . decode) (encode k)
    keys			= fmap fst . toList
    elems 			= T.elems . trie
    toList 			= (L.map (first decode)) . T.toList . trie
    toMap 			= (M.mapKeys decode) . T.toMap . trie
    map f 			= liftST $ T.map f
    mapWithKey f 		= liftST $ T.mapWithKey (f . decode)
    fold f n 			= T.fold f n . trie
    foldWithKey f r 		= T.foldWithKey (f . decode) r . trie
    prefixFind k 		= T.prefixFind (encode k) . trie
    prefixFindWithKey k 	= L.map (first decode) . T.prefixFindWithKey (encode k) . trie
    lookup k                    = T.lookup (encode k) . trie
    union                       = liftST2 $ T.union
    unionWith f                 = liftST2 $ T.unionWith f
    unionWithKey f              = liftST2 $ T.unionWithKey (f . decode)
    difference                  = liftST2 $ T.difference
    differenceWith f            = liftST2 $ T.differenceWith f
    differenceWithKey f         = liftST2 $ T.differenceWithKey (f . decode)

-- ----------------------------------------

instance StringMapFuzzy StringTrie where
    prefixFindNoCase k 		= T.prefixFindBy lower (encode k) . trie
    prefixFindNoCaseWithKey k   = L.map (first decode) . T.prefixFindWithKeyBy lower (encode k) . trie
    lookupNoCase k		= L.map (first decode) . T.lookupBy lower (encode k) . trie

-- ----------------------------------------

instance Eq a => Eq (StringTrie a) where 
  (==) m1 m2			= trie m1 == trie m2

-- ----------------------------------------

instance Ord a => Ord (StringTrie a) where
  compare m1 m2 		= trie m1 `compare` trie m2

-- ----------------------------------------

instance Functor StringTrie where
  fmap 				= map

-- ----------------------------------------

instance Foldable StringTrie where
  foldr 			= fold

-- ----------------------------------------

instance Show a => Show (StringTrie a) where
  show				= show . trie

-- ----------------------------------------

instance Read a => Read (StringTrie a) where
  readsPrec p s			= readsPrec p s >>= return . first ST

-- ----------------------------------------

instance NFData a => NFData (StringTrie a) where
  rnf				= rnf . trie

-- ----------------------------------------

instance (Binary a) => Binary (StringTrie a) where
  put				= put . trie
  get				= get >>= return . ST

-- ----------------------------------------
