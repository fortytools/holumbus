{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.URIs
where

-- import qualified Data.Set       		as S
import qualified Holumbus.Data.PrefixTree	as S

-- ------------------------------------------------------------

-- | An URI is represented as a String
type URI			= String


-- | A set of URIs implemeted as a prefix tree. This implementation
-- is space efficient, because of many equal prefixes in the crawled set of URIs

type URIs			= S.PrefixTree ()

-- ------------------------------------------------------------

emptyURIs		:: URIs
emptyURIs		= S.empty

singletonURIs		:: URI -> URIs
singletonURIs		= flip S.singleton ()

nullURIs		:: URIs -> Bool
nullURIs		= S.null

memberURIs		:: URI -> URIs -> Bool
memberURIs		= S.member

cardURIs		:: URIs -> Int
cardURIs		= S.size

nextURI			:: URIs -> URI
nextURI			= head . toListURIs

nextURIs		:: Int -> URIs -> [URI]
nextURIs n		= take n . toListURIs

insertURI		:: URI -> URIs	-> URIs
insertURI		= flip S.insert ()

deleteURI		:: URI -> URIs	-> URIs
deleteURI		= S.delete

deleteURIs		:: URIs -> URIs	-> URIs
deleteURIs		= flip S.difference

unionURIs		:: URIs -> URIs	-> URIs
unionURIs		= S.union

diffURIs   		:: URIs -> URIs -> URIs
diffURIs   		= S.difference

fromListURIs		:: [URI] -> URIs
fromListURIs		= S.fromList . map (\ x -> (x, ()))

toListURIs		:: URIs -> [URI]
toListURIs		= map fst . S.toList

foldURIs		:: (URI -> b -> b) -> b -> URIs -> b
foldURIs f		= S.foldWithKey (\ x _ r -> f x r)

-- ------------------------------------------------------------


