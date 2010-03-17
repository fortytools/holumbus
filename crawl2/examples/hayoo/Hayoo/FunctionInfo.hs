{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.FunctionInfo
where
import           Control.DeepSeq

import           Data.Binary                    ( Binary )
import qualified Data.Binary                    as B

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

newtype FunctionInfo            = PT { unPT :: String }

instance Binary FunctionInfo where
    get                         = B.get >>= (return . PT)
    put                         = B.put . unPT

instance NFData FunctionInfo where
    rnf                         = rnf . unPT

instance XmlPickler FunctionInfo where
    xpickle                     = xpElem "text" $
                                  xpWrap (PT , unPT)
                                  xpText0

mkFunctionInfo 			:: String -> FunctionInfo
mkFunctionInfo			= PT

-- ------------------------------------------------------------
