-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.StrMap
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.6

  An efficient implementation of maps from string keys to arbitrary values, 
  used for the Holumbus indexes.

  The implementation uses UTF-8 encoding for saving space and is based upon 
  the arbitrary byte trie in "Holumbus.Data.Trie". See this module for
  extensive documentation on most of the functions.
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
  , findWithDefault  
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
  )
where

import Prelude hiding (lookup, map, null)

import Control.Arrow

import Codec.Binary.UTF8.String

import Data.Char

import qualified Data.Map as M
import qualified Data.List as L

import Holumbus.Data.Trie (Trie, Key)
import qualified Holumbus.Data.Trie as T

-- | A map from strings to values a.
type StrMap a = Trie a

(!) :: StrMap a -> String -> a
(!) m k = (T.!) m (encode k)

null :: StrMap a -> Bool
null = T.null

size :: StrMap a -> Int
size = T.size

member :: String -> StrMap a -> Bool
member k m = T.member (encode k) m

empty :: StrMap a
empty = T.empty

singleton :: String -> a -> StrMap a
singleton k v = T.singleton (encode k) v

insertWithKey :: (String -> a -> a -> a) -> String -> a -> StrMap a -> StrMap a
insertWithKey f k v m = T.insertWithKey (f . decode) (encode k) v m

insertWith :: (a -> a -> a) -> String -> a -> StrMap a -> StrMap a
insertWith f k v m = T.insertWith f (encode k) v m

insert :: String -> a -> StrMap a -> StrMap a
insert k v m = T.insert (encode k) v m

delete :: String -> StrMap a -> StrMap a
delete k m = T.delete (encode k) m

update :: (a -> Maybe a) -> String -> StrMap a -> StrMap a
update f k m = T.update f (encode k) m

updateWithKey :: (String -> a -> Maybe a) -> String -> StrMap a -> StrMap a
updateWithKey f k m = T.updateWithKey (f . decode) (encode k) m

elems :: StrMap a -> [a]
elems = T.elems

fromList :: [(String, a)] -> StrMap a
fromList = T.fromList . (L.map (first encode))

toList :: StrMap a -> [(String, a)]
toList = (L.map (first decode)) . T.toList

fromMap :: M.Map String a -> StrMap a
fromMap = T.fromMap . (M.mapKeys encode)

toMap :: StrMap a -> M.Map String a
toMap = (M.mapKeys decode) . T.toMap

map :: (a -> b) -> StrMap a -> StrMap b
map = T.map

mapWithKey :: (String -> a -> b) -> StrMap a -> StrMap b
mapWithKey f m = T.mapWithKey (f . decode) m

fold :: (a -> b -> b) -> b -> StrMap a -> b
fold = T.fold

foldWithKey :: (String -> a -> b -> b) -> b -> StrMap a -> b
foldWithKey f r m = T.foldWithKey (f . decode) r m

prefixFind :: String -> StrMap a -> [a] 
prefixFind = T.prefixFind . encode

prefixFindNoCase :: String -> StrMap a -> [a]
prefixFindNoCase = (T.prefixFindBy lower) . encode

prefixFindWithKey :: String -> StrMap a -> [(String, a)]
prefixFindWithKey = ((L.map (first decode)) .) . T.prefixFindWithKey . encode

prefixFindNoCaseWithKey :: String -> StrMap a -> [(String, a)]
prefixFindNoCaseWithKey = ((L.map (first decode)) .) . (T.prefixFindWithKeyBy lower) . encode

lookup :: Monad m => String -> StrMap a -> m a
lookup = T.lookup . encode

lookupNoCase :: String -> StrMap a -> [(String, a)]
lookupNoCase = ((L.map (first decode)) .) . (T.lookupBy lower) . encode

lower :: Key -> Key
lower = encode . (L.map toLower) . decode

findWithDefault :: a -> String -> StrMap a -> a
findWithDefault v k m = T.findWithDefault v (encode k) m

union :: StrMap a -> StrMap a -> StrMap a
union = T.union

unionWith :: (a -> a -> a) -> StrMap a -> StrMap a -> StrMap a
unionWith = T.unionWith

unionWithKey :: (String -> a -> a -> a) -> StrMap a -> StrMap a -> StrMap a
unionWithKey f = T.unionWithKey (f . decode)

difference :: StrMap a -> StrMap b -> StrMap a
difference = T.difference

differenceWith :: (a -> b -> Maybe a) -> StrMap a -> StrMap b -> StrMap a
differenceWith = T.differenceWith

differenceWithKey :: (String -> a -> b -> Maybe a) -> StrMap a -> StrMap b -> StrMap a
differenceWithKey f = T.differenceWithKey (f . decode)
