{-# OPTIONS #-}
{-# OPTIONS -XBangPatterns #-}

-- ------------------------------------------------------------

module Hayoo.PackageInfo
where
import           Control.DeepSeq
import 		 Control.Monad			( liftM4 )

import           Data.Binary                    ( Binary(..) )
import qualified Data.Binary                    as B
import           Data.ByteString.UTF8           (ByteString)
import qualified Data.ByteString.UTF8           as S

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

-- | Additional information about a function.

data PackageInfo 		= PackageInfo 
    				  { p_version 		:: ! ByteString      	-- ^ The latest package version
				  , p_dependencies	:: ! ByteString       	-- ^ The list of required packages
				  , p_maintainer	:: ! ByteString		-- ^ The maintainer
				  , p_category		:: ! ByteString		-- ^ The package category
				  , p_homepage		:: ! ByteString		-- ^ The home page
				  , p_description	:: ! ByteString		-- ^ The description of the package
				  } 
				  deriving (Show, Eq)

mkPackageInfo 			:: String -> [String] -> String -> String -> String -> String -> PackageInfo
mkPackageInfo v d m c h s	= PackageInfo
				  (S.fromString v) (S.fromString . unwords d) (S.fromString m)
				  (S.fromString c) (S.fromString h) (S.fromString s)

instance XmlPickler PackageInfo where
    xpickle 			= xpWrap (fromTuple, toTuple) xpPackage
	where
	fromTuple (v, d, m, c, h, s)
			 	= PackageInfo
				  (S.fromString v) (S.fromString d) (S.fromString m)
				  (S.fromString c) (S.fromString h) (S.fromString s)
	toTuple (PackageInfo v d m c h s)
				= (S.toString v, S.toString d, S.toString m, S.toString c, S.toString h, S.toString s)
	xpPackage		= xp6Tuple xpVersion xpDependencies xpMaintainer xpCategory xpHomepage xpDescr
	    where 				
	    xpVersion		= xpAttr "version"      xpText0
	    xpDependencies	= xpAttr "dependencies" xptext0
	    xpMaintainer	= xpAttr "maintainer"   xpText0
	    xpCategory		= xpAttr "category"     xpText0
	    xpHomepage		= xpAttr "homepage"     xpText0
	    xpDescr		= xpText

instance NFData PackageInfo where
  rnf (PackageInfo v d m c h s)	= S.length v `seq` S.length d `seq` S.length m `seq`
				  S.length c `seq` S.length h `seq` S.length s `seq` ()

instance B.Binary PackageInfo where
    put (PackageInfo v d m c h s)
			 	= put v >> put d >> put m >> put c >> put h >> put s
    get 			= do
				  r <- liftM6 PackageInfo get get get get get get
				  return $! r

-- ------------------------------------------------------------
