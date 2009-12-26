
-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Class
  Copyright  : Copyright (C) 2009 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A class for Maps of Strings to values
  with prefix search functions.

  Names in this classe are similar to those in Data.Map, Data.IntMap and some Prelude names,
  when using this class the other modules must be imported qualified, and the Prelude names
  must be hidden or also imported qualified. 
-}

-- ----------------------------------------------------------------------------

module Data.StringMap.Class
where

import           Prelude 		hiding (lookup, map, null)

import qualified Data.List		as L
import qualified Data.Map 		as M
import           Data.Maybe

-- ----------------------------------------

class StringMap m where
    null 		:: m a -> Bool

    size 		:: m a -> Int
    size 		= fold (const (+1)) 0

    insert 		:: String -> a -> m a -> m a
    insert 		= insertWith const

    insertWith 		:: (a -> a -> a) -> String -> a -> m a -> m a

    insertWithKey 	:: (String -> a -> a -> a) -> String -> a -> m a -> m a
    insertWithKey f k 	= insertWith (f k) k

    delete 		:: String -> m a -> m a
    delete		= update (const Nothing)

    update 		:: (a -> Maybe a) -> String -> m a -> m a

    updateWithKey 	:: (String -> a -> Maybe a) -> String -> m a -> m a
    updateWithKey f k	= update (f k) k

    keys		:: m a -> [String]
    keys		= foldWithKey (\ k _v r -> k : r) []

    elems 		:: m a -> [a]
    elems   		= fold (:) []

    toList 		:: m a -> [(String, a)]
    toList		= foldWithKey (\k v r -> (k, v) : r) []

    toMap 		:: m a -> M.Map String a
    toMap 		= foldWithKey M.insert M.empty

    map 		:: (a -> b) -> m a -> m b
    map f 		= mapWithKey (const f)

    mapWithKey 		:: (String -> a -> b) -> m a -> m b

    fold 		:: (a -> b -> b) -> b -> m a -> b
    fold f 		= foldWithKey $ const f

    foldWithKey 	:: (String -> a -> b -> b) -> b -> m a -> b

    prefixFind 		:: String -> m a -> [a] 
    prefixFindWithKey 	:: String -> m a -> [(String, a)]

    member 		:: String -> m a -> Bool
    member k 		= maybe False (const True) . lookup k

    (!) 		:: m a -> String -> a
    (!)	 		= flip $ findWithDefault (error "StringMap.! : element not in the map")

    lookup 		:: Monad m1 => String -> m a -> m1 a

    findWithDefault 	:: a -> String -> m a -> a
    findWithDefault v k	= fromMaybe v . lookup k

    union 		:: m a -> m a -> m a
    unionWith 		:: (a -> a -> a) -> m a -> m a -> m a
    unionWithKey 	:: (String -> a -> a -> a) -> m a -> m a -> m a

    difference 		:: m a -> m b -> m a
    difference		= differenceWith (const (const Nothing))

    differenceWith 	:: (a -> b -> Maybe a) -> m a -> m b -> m a
    differenceWith f 	= differenceWithKey (const f)

    differenceWithKey 	:: (String -> a -> b -> Maybe a) -> m a -> m b -> m a

-- ----------------------------------------

class StringMapFuzzy m where
    prefixFindNoCase 		:: String -> m a -> [a]
    prefixFindNoCaseWithKey 	:: String -> m a -> [(String, a)]
    lookupNoCase 		:: String -> m a -> [(String, a)]

-- ----------------------------------------

class StringMap m => StringMapConstructors m where
    empty			:: m a

    singleton 			:: String -> a -> m a
    singleton k v 		= insert k v empty

    fromList 		        :: [(String, a)] -> m a
    fromList		        = L.foldl' (\p (k, v) -> insert k v p) empty

    fromMap 			:: M.Map String a -> m a
    fromMap			= M.foldWithKey insert empty

-- ----------------------------------------
