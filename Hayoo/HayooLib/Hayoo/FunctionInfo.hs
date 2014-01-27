{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- ------------------------------------------------------------

module Hayoo.FunctionInfo
    ( FunctionInfo(..)
    , mkFunctionInfo
    )
where
import           Control.Applicative ((<$>), (<*>))
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary         (Binary (..))
import qualified Data.Binary         as B
import           Data.Maybe
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
      , fctType    :: ! Fct'Type           -- ^ The type of the documented part, function, class, type, ...
      }
    deriving (Show, Eq, Typeable)

data Fct'Type
    = Fct'class
    | Fct'data
    | Fct'function
    | Fct'method
    | Fct'module
    | Fct'newtype
    | Fct'type
    | Fct'unknown
      deriving (Show, Eq, Enum, Bounded, Typeable)

fctAssocList :: [(String, Fct'Type)]
fctAssocList = map (\ x -> (drop 4 . show $ x, x)) [minBound..maxBound]

toFct'Type :: String -> Fct'Type
toFct'Type s = fromMaybe Fct'unknown $ lookup s fctAssocList

fromFct'Type :: Fct'Type -> String
fromFct'Type = drop 4 . show

-- mkFunctionInfo is a strict constructor

mkFunctionInfo                  :: String -> String -> String -> String -> String -> String -> FunctionInfo
mkFunctionInfo m s p r d t      = let res = FunctionInfo m s p r d (toFct'Type t) in
                                  rnf res `seq` res

instance XmlPickler FunctionInfo where
    xpickle                     = xpWrap (fromTuple, toTuple) xpFunction
        where
        fromTuple (m, s, p, r, d, t)
                                = FunctionInfo m s p r d t
        toTuple (FunctionInfo m s p r d t)
                                = (m, s, p, r, d, t)

        xpFunction              = xp6Tuple xpModule xpSignature xpPackage xpSource xpDescr xpType
            where               -- We are inside a doc-element, and everything is stored as attribute.
            xpModule            = xpAttr "module"    xpText0
            xpSignature         = xpAttr "signature" xpText0
            xpPackage           = xpAttr "package"   xpText0
            xpSource            = xpAttr "source"    xpText0
            xpDescr             = xpAttr "descr"     xpText0
            xpType              = xpAttr "type"      xpickle

instance XmlPickler Fct'Type where
    xpickle                     = xpWrap (toFct'Type, fromFct'Type) xpText0

instance NFData FunctionInfo where
    rnf (FunctionInfo m s p r d _t)
                                = rnf m `seq` rnf s `seq` rnf p `seq` rnf r `seq` rnf d

instance B.Binary FunctionInfo where
    put (FunctionInfo m s p r d t)
                                = put m >> put s >> put p >> put r >> put d >> put t
    get                         = do
                                  r <- FunctionInfo <$> get <*> get <*> get <*> get <*> get <*> get
                                  rnf r `seq`
                                      return r

instance B.Binary Fct'Type where
    put = put . fromEnum
    get = toEnum <$> get

instance ToJSON FunctionInfo where
    toJSON (FunctionInfo mon sig pac sou fct typ)
        = object
          ( map (uncurry (.=)) . filter (not . null . snd)
            $ [ ("fct-module",    mon)
              , ("fct-signature", sig)
              , ("fct-package",   pac)
              , ("fct-source",    sou)
              , ("fct-descr",     fct)
              , ("fct-type",      fromFct'Type typ)
              ]
          )

instance Sizeable FunctionInfo where
    dataOf _x                   = 6 .*. dataOfPtr
    statsOf x@(FunctionInfo m s p r d _t)
                                = mkStats x <> statsOf m <> statsOf s <> statsOf p
                                            <> statsOf r <> statsOf d

-- ------------------------------------------------------------
