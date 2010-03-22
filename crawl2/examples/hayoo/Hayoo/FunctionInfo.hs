{-# OPTIONS #-}
{-# OPTIONS -XBangPatterns #-}

-- ------------------------------------------------------------

module Hayoo.FunctionInfo
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

data FunctionInfo 		= FunctionInfo 
    				  { moduleName 	:: ! ByteString      	-- ^ The name of the module containing the function, e.g. Data.Map
				  , signature 	:: ! ByteString       	-- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
				  , package   	:: ! ByteString       	-- ^ The name of the package containing the module, e.g. containers
				  , sourceURI 	:: ! ByteString 	-- ^ An optional URI to the online source of the function.
				  } 
				  deriving (Show, Eq)

mkFunctionInfo 			:: String -> String -> String -> String -> FunctionInfo
mkFunctionInfo m s p r		= FunctionInfo (S.fromString m) (S.fromString s) (S.fromString p) (S.fromString r)

instance XmlPickler FunctionInfo where
    xpickle 			= xpWrap (fromTuple, toTuple) xpFunction
	where
	fromTuple (m, s, p, r) 	= FunctionInfo (S.fromString m) (S.fromString s) (S.fromString p) (S.fromString r)
	toTuple (FunctionInfo m s p r)
				= (S.toString m, S.toString s, S.toString p, S.toString r)
	xpFunction		= xp4Tuple xpModule xpSignature xpPackage xpSource
	    where 							-- We are inside a doc-element, therefore everything is stored as attribute.
	    xpModule 		= xpAttr "module"    xpText0
	    xpSignature 	= xpAttr "signature" xpText0
	    xpPackage 		= xpAttr "package"   xpText0
	    xpSource 		= xpAttr "source"    xpText0

instance NFData FunctionInfo where
  rnf (FunctionInfo m s p r)	= S.length m `seq` S.length s `seq` S.length p `seq` S.length r `seq` ()

instance B.Binary FunctionInfo where
    put (FunctionInfo m s p r) 	= put m >> put s >> put p >> put r
    get 			= do
				  r <- liftM4 FunctionInfo get get get get
				  return $! r

-- ------------------------------------------------------------
