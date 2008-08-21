-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Data.KeyMap
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Holumbus.Data.KeyMap
(
  KeyMap
, Key(..)
, empty
, null
, insert
, lookup
, keys
, elems
, memberKey
, memberElem
, deleteKey
, deleteElem
, fromList
, toList
, toAscList
 
)
where

import           Prelude hiding (null, lookup)

import qualified Data.Map as Map


-- | every element of this map has to implement a key-function. which
--   gives us the key of the element
class Key n where
  getKey :: n -> String


data KeyMap a = NM (Map.Map String a)
  deriving (Show, Eq, Ord)

-- | the empty KeyMap
empty :: KeyMap a
empty = NM Map.empty


-- | test, if the MultiMap is empty
null :: (Key a) => KeyMap a -> Bool
null (NM m) = Map.null m


-- | inserts an element in the KeyMap
insert :: (Key a) => a -> KeyMap a -> KeyMap a
insert a (NM m) = NM $ Map.insert (getKey a) a m
  

-- | gets all different elements for one key or an empty set
lookup :: (Key a, Monad m) => String -> KeyMap a -> m a
lookup k (NM m) = Map.lookup k m


keys :: (Key a) => KeyMap a -> [String]
keys (NM m) = Map.keys m


elems :: (Key a) => KeyMap a -> [a]
elems (NM m) = Map.elems m


-- | test, if a key is in the Map
memberKey :: (Key a) => String -> KeyMap a -> Bool
memberKey k (NM m) = Map.member k m


-- | test, if a key is in the Map
memberElem :: (Key a) => a -> KeyMap a -> Bool
memberElem a (NM m) = Map.member (getKey a) m


-- | deletes a whole key from the map
deleteKey :: (Key a) => String -> KeyMap a -> KeyMap a
deleteKey k (NM m) = NM $ Map.delete k m


-- | deletes a single Elemete from the map
deleteElem :: (Key a) => a -> KeyMap a -> KeyMap a
deleteElem a (NM m) = NM $ Map.delete (getKey a) m


fromList :: (Key a) => [a] -> KeyMap a
fromList as = foldl (\m a -> insert a m) empty as


toList :: (Key a) => KeyMap a -> [a]
toList (NM m) = map (snd) $ Map.toList m


toAscList :: (Key a) => KeyMap a -> [a]
toAscList (NM m) = map (snd) $ Map.toAscList m