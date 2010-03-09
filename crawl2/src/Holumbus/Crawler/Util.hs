{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Util
where

import		 Control.Applicative				( liftA2 )

import           Data.List

import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch	( match )

-- ------------------------------------------------------------

-- | create temp file name

mkTmpFile			:: Int -> String -> Int -> String
mkTmpFile n s i			= (s ++) . reverse . take n . (++ replicate n '0') . reverse . show $ i

-- ------------------------------------------------------------

-- | Simple predicate genertor for filtering of URIs
-- If the first predicate (isAllowed) holds and the second (isDenied) does not hold
-- the predicate holds. This can be used for constructing simple URL filters

simpleFollowRef			:: (String -> Bool) -> (String -> Bool) -> (String -> Bool)
simpleFollowRef isAllowed isDenied
    				= isAllowed .&&. (not . isDenied)
				  where
				  (.&&.) = liftA2 (&&)

-- | A convenient function, that takes two lists strings in regexp syntax,
-- The first list are the patterns for the allowed strings,
-- the second one for the patterns to deny the string.
-- Two regular expressions are build from these lists of strings,
-- and the string to be tested is matched against both regexes

simpleFollowRef'		:: [String] -> [String] -> (String -> Bool)
simpleFollowRef' allowed denied
				= simpleFollowRef allowed' denied'
    where
    mkAlt			:: [String] -> String
    mkAlt rs			= "(" ++ intercalate "|" rs ++ ")"
    allowed'
        | null allowed		= const True
        | otherwise		= match $ mkAlt allowed
    denied'
        | null denied		= const False
        | otherwise		= match $ mkAlt denied

-- ------------------------------------------------------------
