{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.URIs
where

import qualified Data.Set       		as S

-- ------------------------------------------------------------

-- | An URI is represented as a String
type URI			= String


-- | A set of URIs
type URIs			= S.Set URI

-- ------------------------------------------------------------

emptyURIs		:: URIs
emptyURIs		= S.empty

singletonURIs		:: URI -> URIs
singletonURIs		= S.singleton

nullURIs		:: URIs -> Bool
nullURIs		= S.null

memberURIs		:: URI -> URIs -> Bool
memberURIs		= S.member

cardURIs		:: URIs -> Int
cardURIs		= S.size

nextURI			:: URIs -> URI
nextURI			= S.findMin

nextURIs		:: Int -> URIs -> [URI]
nextURIs n		= take n . S.toList

insertURI		:: URI -> URIs	-> URIs
insertURI		= S.insert

deleteURI		:: URI -> URIs	-> URIs
deleteURI		= S.delete

deleteURIs		:: URIs -> URIs	-> URIs
deleteURIs		= flip S.difference

unionURIs		:: URIs -> URIs	-> URIs
unionURIs		= S.union

diffURIs   		:: URIs -> URIs -> URIs
diffURIs   		= S.difference

fromListURIs		:: [URI] -> URIs
fromListURIs		= S.fromList

toListURIs		:: URIs -> [URI]
toListURIs		= S.toList

foldURIs		:: (URI -> b -> b) -> b -> URIs -> b
foldURIs		= S.fold

-- ------------------------------------------------------------


