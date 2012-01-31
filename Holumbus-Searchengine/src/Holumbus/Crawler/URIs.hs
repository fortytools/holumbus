{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.URIs
where

import qualified Holumbus.Data.PrefixTree       as S

-- ------------------------------------------------------------

-- | An URI is represented as a String
type URI                        = String
type URIWithLevel               = (URI, Int)

-- | A set of URIs implemeted as a prefix tree. This implementation
-- is space efficient, because of many equal prefixes in the crawled set of URIs

type URIs                       = URIs' ()
type URIsWithLevel              = URIs' Int     -- URIs with a priority or clicklevel

type URIs' a                    = S.PrefixTree a

-- ------------------------------------------------------------

emptyURIs               :: URIs' a
emptyURIs               = S.empty

singletonURIs           :: URI -> URIs
singletonURIs           = flip S.singleton ()

singletonURIs'          :: URI -> a -> URIs' a
singletonURIs'          = S.singleton

nullURIs                :: URIs' a -> Bool
nullURIs                = S.null

memberURIs              :: URI -> URIs' a -> Bool
memberURIs              = S.member

cardURIs                :: URIs' a -> Int
cardURIs                = S.size

nextURI                 :: URIs' a -> (URI, a)
nextURI                 = head . toListURIs'

nextURIs                :: Int -> URIs' a -> [(URI, a)]
nextURIs n              = take n . toListURIs'

insertURI               :: URI -> URIs  -> URIs
insertURI               = flip S.insert ()

insertURI'              :: URI -> a -> URIs' a -> URIs' a
insertURI'              = S.insert

deleteURI               :: URI -> URIs' a -> URIs' a
deleteURI               = S.delete

deleteURIs              :: URIs' b -> URIs' a -> URIs' a
deleteURIs              = flip S.difference

unionURIs               :: URIs -> URIs -> URIs
unionURIs               = S.union

unionURIs'              :: (a -> a -> a) -> URIs' a -> URIs' a -> URIs' a
unionURIs'              = S.unionWith

diffURIs                :: URIs' a -> URIs' a -> URIs' a
diffURIs                = S.difference

fromListURIs            :: [URI] -> URIs
fromListURIs            = S.fromList . map (\ x -> (x, ()))

fromListURIs'           :: [(URI, a)] -> URIs' a
fromListURIs'           = S.fromList

toListURIs              :: URIs' a -> [URI]
toListURIs              = S.keys                -- map fst . S.toList

toListURIs'             :: URIs' a -> [(URI, a)]
toListURIs'             = S.toList

foldURIs                :: (URI -> b -> b) -> b -> URIs -> b
foldURIs f              = S.foldWithKey (\ x _ r -> f x r)

foldURIs'               :: (URI -> a -> b -> b) -> b -> URIs' a -> b
foldURIs'               = S.foldWithKey

-- ------------------------------------------------------------


