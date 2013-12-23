{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ------------------------------------------------------------

module Hayoo.FunctionInfo
    ( FunctionInfo(..)
    , mkFunctionInfo
    )
where
import           Control.DeepSeq
import           Control.Monad     (liftM5)

import           Data.Binary       (Binary (..))
import qualified Data.Binary       as B
import           Data.Size
import           Data.Typeable

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | Additional information about a function.

data FunctionInfo
    = FunctionInfo
      { moduleName :: String               -- ^ The name of the module containing the function, e.g. Data.Map
      , signature  :: String               -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
      , package    :: String               -- ^ The name of the package containing the module, e.g. containers
      , sourceURI  :: String               -- ^ An optional URI to the online source of the function.
      , fctDescr   :: String               -- ^ The haddock description of a type or function, maybe shortened for space efficiency
      }
    deriving (Show, Eq, Typeable)

mkFunctionInfo                  :: String -> String -> String -> String -> String -> FunctionInfo
mkFunctionInfo                  = FunctionInfo

instance XmlPickler FunctionInfo where
    xpickle                     = xpWrap (fromTuple, toTuple) xpFunction
        where
        fromTuple (m, s, p, r, d)
                                = FunctionInfo m s p r d
        toTuple (FunctionInfo m s p r d)
                                = (m, s, p, r, d)

        xpFunction              = xp5Tuple xpModule xpSignature xpPackage xpSource xpDescr
            where               -- We are inside a doc-element, and everything is stored as attribute.
            xpModule            = xpAttr "module"    xpText0
            xpSignature         = xpAttr "signature" xpText0
            xpPackage           = xpAttr "package"   xpText0
            xpSource            = xpAttr "source"    xpText0
            xpDescr             = xpAttr "descr"     xpText0

instance NFData FunctionInfo where
  rnf (FunctionInfo m s p r d)  = rnf m `seq` rnf s `seq` rnf p `seq` rnf r `seq` rnf d

instance B.Binary FunctionInfo where
    put (FunctionInfo m s p r d)
                                = put m >> put s >> put p >> put r >> put d
    get                         = do
                                  r <- liftM5 FunctionInfo get get get get get
                                  rnf r `seq`
                                      return r

instance Sizeable FunctionInfo where
    dataOf _x                   = 5 .*. dataOfPtr
    statsOf x@(FunctionInfo m s p r d)
                                = mkStats x <> statsOf m <> statsOf s <> statsOf p
                                            <> statsOf r <> statsOf d

-- ------------------------------------------------------------
