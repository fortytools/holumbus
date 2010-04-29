{-# OPTIONS #-}
{-# OPTIONS -XBangPatterns #-}

-- ------------------------------------------------------------

module Hayoo.PackageInfo
where

import           Control.DeepSeq

import           Data.Binary                    ( Binary(..) )
import qualified Data.Binary                    as B
import           Data.ByteString.UTF8           (ByteString)
import qualified Data.ByteString.UTF8           as S

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

-- | Additional information about a function.

data PackageInfo 		= PackageInfo 
    				  { p_name		:: ! ByteString		-- ^ The name of the package
                                  , p_version 		:: ! ByteString      	-- ^ The latest package version
				  , p_dependencies	:: ! ByteString       	-- ^ The list of required packages
                                  , p_author		:: ! ByteString		-- ^ The author
				  , p_maintainer	:: ! ByteString		-- ^ The maintainer
				  , p_category		:: ! ByteString		-- ^ The package category
				  , p_homepage		:: ! ByteString		-- ^ The home page
                                  , p_synopsis		:: ! ByteString		-- ^ The synopsis
				  , p_description	:: ! ByteString		-- ^ The description of the package
				  } 
				  deriving (Show, Eq)

mkPackageInfo 			:: String -> String -> [String] -> String -> String -> String -> String -> String -> String -> PackageInfo
mkPackageInfo n v d a m c h y s	= PackageInfo
				  (S.fromString n) (S.fromString v) (S.fromString . unwords $ d) (S.fromString a)
				  (S.fromString m) (S.fromString c) (S.fromString h) (S.fromString y) (S.fromString s)

instance XmlPickler PackageInfo where
    xpickle 			= xpWrap (fromTuple, toTuple) xpPackage
	where
	fromTuple ((n, v, d), a, m, c, h, (y, s))
			 	= PackageInfo
				  (S.fromString n) (S.fromString v) (S.fromString d) (S.fromString a)
				  (S.fromString m) (S.fromString c) (S.fromString h)
                                  (S.fromString y) (S.fromString s)
	toTuple (PackageInfo n v d a m c h y s)
				= ((S.toString n, S.toString v, S.toString d)
                                  , S.toString a, S.toString m, S.toString c, S.toString h
                                  ,(S.toString y, S.toString s)
                                  )
	xpPackage		= xp6Tuple
                                  (xpTriple xpName xpVersion xpDependencies)
                                  xpAuthor xpMaintainer xpCategory xpHomepage
                                  (xpPair xpSynopsis xpDescr)
	    where 				
	    xpName		= xpAttr "name"         xpText0
	    xpVersion		= xpAttr "version"      xpText0
	    xpDependencies	= xpAttr "dependencies" xpText0
	    xpAuthor		= xpAttr "author"       xpText0
	    xpMaintainer	= xpAttr "maintainer"   xpText0
	    xpCategory		= xpAttr "category"     xpText0
	    xpHomepage		= xpAttr "homepage"     xpText0
	    xpSynopsis		= xpAttr "synopsis"     xpText0
	    xpDescr		= xpText

instance NFData PackageInfo where
  rnf (PackageInfo n v d a m c h y s)
				= S.length n `seq` S.length v `seq` S.length d `seq` S.length a `seq`
				  S.length m `seq` S.length c `seq` S.length h `seq` S.length y `seq`
                                  S.length s `seq` ()

instance B.Binary PackageInfo where
    put (PackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9)
			 	= put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6 >> put x7 >> put x8 >> put x9
    get 			= do
                                  x1 <- get
                                  x2 <- get
                                  x3 <- get
                                  x4 <- get
                                  x5 <- get
                                  x6 <- get
                                  x7 <- get
                                  x8 <- get
                                  x9 <- get
                                  return $! PackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9

-- ------------------------------------------------------------
