
 {-# LANGUAGE BangPatterns #-} 

import Holumbus.Index.Common

import Holumbus.Index.Documents (Documents)
import qualified Holumbus.Index.Documents as D

import Holumbus.Index.SmallDocuments (SmallDocuments)
import qualified Holumbus.Index.SmallDocuments as SD

import qualified Data.ByteString.UTF8 as B
import qualified Data.IntMap as IM

import Control.Monad hiding (join)

import Text.XML.HXT.Arrow

import Data.Binary

import Hayoo.Common

-- | Additional information about a function.
data OldFunctionInfo = OldFunctionInfo 
  { moduleName :: String      -- ^ The name of the module containing the function, e.g. Data.Map
  , signature :: String       -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
  , package   :: String       -- ^ The name of the package containing the module, e.g. containers
  , sourceURI :: Maybe String -- ^ An optional URI to the online source of the function.
  } 
  deriving (Show, Eq)

instance XmlPickler OldFunctionInfo where
  xpickle = xpWrap (fromTuple, toTuple) xpFunction
    where
    fromTuple (m, s, p, r) = OldFunctionInfo m s p r
    toTuple (OldFunctionInfo m s p r) = (m, s, p, r)
    xpFunction = xp4Tuple xpModule xpSignature xpPackage xpSource
      where -- We are inside a doc-element, therefore everything is stored as attribute.
      xpModule = xpAttr "module" xpText0
      xpSignature = xpAttr "signature" xpText0
      xpPackage = xpAttr "package" xpText0
      xpSource = xpOption (xpAttr "source" xpText0)

instance Binary OldFunctionInfo where
  put (OldFunctionInfo m s p r) = put m >> put s >> put p >> put r
  get = liftM4 OldFunctionInfo get get get get

convFunctionInfo :: OldFunctionInfo -> FunctionInfo
convFunctionInfo (OldFunctionInfo m s p r) = FunctionInfo (B.fromString m) (B.fromString s) (B.fromString p) (liftM B.fromString $ r)

main :: IO ()
main = do
       d <- loadFromFile "docs.bin" :: IO (Documents OldFunctionInfo)
       sd <- return (fromMap . (IM.map (\(Document t u c) -> Document t u (liftM convFunctionInfo $ c))) . toMap $ d) :: IO (SmallDocuments FunctionInfo)
       writeToBinFile "docs-small.bin" sd
