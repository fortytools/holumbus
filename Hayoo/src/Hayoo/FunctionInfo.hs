{-# OPTIONS #-}
{-# OPTIONS -XBangPatterns #-}

-- ------------------------------------------------------------

module Hayoo.FunctionInfo
    ( FunctionInfo(..)
    , mkFunctionInfo
    )
where
import           Control.DeepSeq
import 		 Control.Monad			( liftM5 )

import           Data.Binary                    ( Binary(..) )
import qualified Data.Binary                    as B
-- import           Data.ByteString.UTF8           (ByteString)
-- import qualified Data.ByteString.UTF8           as S
-- import qualified Data.ByteString                as S ( empty )

import		 Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | Additional information about a function.

data FunctionInfo 		= FunctionInfo 
    				  { moduleName 	:: String      		-- ^ The name of the module containing the function, e.g. Data.Map
				  , signature 	:: String       	-- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
				  , package   	:: String       	-- ^ The name of the package containing the module, e.g. containers
				  , sourceURI 	:: String	 	-- ^ An optional URI to the online source of the function.
				  , fctDescr	:: String		-- ^ The haddock description of a type or function, maybe shortened for space efficiency
				  } 
				  deriving (Show, Eq)

mkFunctionInfo 			:: String -> String -> String -> String -> String -> FunctionInfo
mkFunctionInfo			= FunctionInfo

instance XmlPickler FunctionInfo where
    xpickle 			= xpWrap (fromTuple, toTuple) xpFunction
	where
	fromTuple (m, s, p, r, d)
			 	= FunctionInfo m s p r d
	toTuple (FunctionInfo m s p r d)
				= (m, s, p, r, d)

	xpFunction		= xp5Tuple xpModule xpSignature xpPackage xpSource xpDescr
	    where 							-- We are inside a doc-element, and everything is stored as attribute.
	    xpModule 		= xpAttr "module"    xpText0
	    xpSignature 	= xpAttr "signature" xpText0
	    xpPackage 		= xpAttr "package"   xpText0
	    xpSource 		= xpAttr "source"    xpText0
	    xpDescr		= xpAttr "descr"     xpText0

instance NFData FunctionInfo where
  rnf (FunctionInfo m s p r d)	= rnf m `seq` rnf s `seq` rnf p `seq` rnf r `seq` rnf d `seq` ()

instance B.Binary FunctionInfo where
    put (FunctionInfo m s p r d)
			 	= put m >> put s >> put p >> put r >> put d
    get 			= do
				  r <- liftM5 FunctionInfo get get get get get
				  rnf r `seq` return r

-- ------------------------------------------------------------
{- The document descriptions are serialized and compressed when put into the document table,
 so there is no need for optimizing the srings in the function info record

-- | Additional information about a function.

data FunctionInfo 		= FunctionInfo 
    				  { moduleName 	:: ! ByteString      	-- ^ The name of the module containing the function, e.g. Data.Map
				  , signature 	:: ! ByteString       	-- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
				  , package   	:: ! ByteString       	-- ^ The name of the package containing the module, e.g. containers
				  , sourceURI 	:: ! ByteString 	-- ^ An optional URI to the online source of the function.
				  , fctDescr	:: ! ByteString		-- ^ The haddock description of a type or function, maybe shortened for space efficiency
				  } 
				  deriving (Show, Eq)

mkFunctionInfo 			:: String -> String -> String -> String -> String -> FunctionInfo
mkFunctionInfo m s p r d	= FunctionInfo (fromString m) (fromString s) (fromString p) (fromString r) (fromString d)

fromString			:: String -> ByteString
fromString ""			= S.empty				-- space optimization
fromString s			= S.fromString s

toString			:: ByteString -> String
toString			= S.toString

instance XmlPickler FunctionInfo where
    xpickle 			= xpWrap (fromTuple, toTuple) xpFunction
	where
	fromTuple (m, s, p, r, d)
			 	= FunctionInfo (fromString m) (fromString s) (fromString p) (fromString r) (fromString d)
	toTuple (FunctionInfo m s p r d)
				= (toString m, toString s, toString p, toString r, toString d)

	xpFunction		= xp5Tuple xpModule xpSignature xpPackage xpSource xpDescr
	    where 							-- We are inside a doc-element, and everything is stored as attribute.
	    xpModule 		= xpAttr "module"    xpText0
	    xpSignature 	= xpAttr "signature" xpText0
	    xpPackage 		= xpAttr "package"   xpText0
	    xpSource 		= xpAttr "source"    xpText0
	    xpDescr		= xpAttr "descr"     xpText0

instance NFData FunctionInfo where
  rnf (FunctionInfo m s p r d)	= S.length m `seq` S.length s `seq` S.length p `seq` S.length r `seq` S.length d `seq` ()

instance B.Binary FunctionInfo where
    put (FunctionInfo m s p r d)
			 	= put m >> put s >> put p >> put r >> put d
    get 			= do
				  
				  r <- liftM5 FunctionInfo get get get get get
				  return $! r
-}
-- ------------------------------------------------------------
