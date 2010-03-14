-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Data.KeyMap
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The KeyMap is derived from the Data.Map type. The keys of the Map are
  strings and the values can be arbitrary data objects. But you don't have to
  specify the keys because every value-object is able to create it's own
  key via the "Key" typeclass.
  
  From the functionality, the KeyMap stands between a set and a Map. If you
  want to insert an element to the map, it behaves like a set. You don't need 
  an additionnal key and it makes no different if you insert an object multiple
  times. If you want to access the objects in the KeyMap, you can lookup them
  via the key, so in this case this container behaves like an ordinary map.

  The functions for this container are named after the standard Map and Set
  functions.
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

-- | Every element of this map has to implement a key-function. which
--   gives us the key of the element
class Key n where
  getKey :: n -> String


-- | The KeyMap datatype.
data KeyMap a = NM (Map.Map String a)
  deriving (Show, Eq, Ord)


-- | The empty KeyMap.
empty :: KeyMap a
empty = NM Map.empty


-- | Test, if the MultiMap is empty.
null :: (Key a) => KeyMap a -> Bool
null (NM m) = Map.null m


-- | Inserts an element in the KeyMap.
insert :: (Key a) => a -> KeyMap a -> KeyMap a
insert a (NM m) = NM $ Map.insert (getKey a) a m
  

-- | Gets all different elements for one key or an empty set.
lookup :: (Key a, Monad m) => String -> KeyMap a -> m a
lookup k (NM mm)
    = do
      let Just r = Map.lookup k mm
      return r

-- | Get all different keys from the map.
keys :: (Key a) => KeyMap a -> [String]
keys (NM m) = Map.keys m


-- | Get all different values in the map without regarding their keys.
elems :: (Key a) => KeyMap a -> [a]
elems (NM m) = Map.elems m


-- | Test, if a key is in the KeyMap.
memberKey :: (Key a) => String -> KeyMap a -> Bool
memberKey k (NM m) = Map.member k m


-- | Test, if a data object is in the KeyMap.
memberElem :: (Key a) => a -> KeyMap a -> Bool
memberElem a (NM m) = Map.member (getKey a) m


-- | Deletes a whole key from the KeyMap.
deleteKey :: (Key a) => String -> KeyMap a -> KeyMap a
deleteKey k (NM m) = NM $ Map.delete k m


-- | Deletes a single elemet from the KeyMap.
deleteElem :: (Key a) => a -> KeyMap a -> KeyMap a
deleteElem a (NM m) = NM $ Map.delete (getKey a) m


-- | Creates a KeyMap from a list of keys.
fromList :: (Key a) => [a] -> KeyMap a
fromList as = foldl (\m a -> insert a m) empty as


-- | Transforms a KeyMap to a list of keys.
toList :: (Key a) => KeyMap a -> [a]
toList (NM m) = map (snd) $ Map.toList m


-- | The same as toList, but the keys are in ascending order.
toAscList :: (Key a) => KeyMap a -> [a]
toAscList (NM m) = map (snd) $ Map.toAscList m
