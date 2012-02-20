{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.DocId
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The document identifier type DocId

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.DocId
where

import Control.DeepSeq

import Data.Binary              ( Binary (..) )
import qualified
       Data.Binary              as B
import Data.Word                ( Word64 )

import Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | The unique identifier of a document
-- (created upon insertion into the document table).

newtype DocId                   = DocId { theDocId :: Word64 }
                                  deriving (Eq, Ord, Enum)
instance Show DocId where
    show                        = show . theDocId

instance NFData DocId where
    rnf (DocId i)               = rnf i

instance Binary DocId where
    put                         = B.put . theDocId
    get                         = B.get >>= return . DocId

incrDocId                       :: DocId -> DocId
incrDocId                       = DocId . (1+) . theDocId

nullDocId                       :: DocId
nullDocId                       = DocId 0

firstDocId                      :: DocId
firstDocId                      = DocId 1

mkDocId                         :: Word64 -> DocId
mkDocId                         = DocId

xpDocId                         :: PU DocId
xpDocId                         = xpWrap (DocId, theDocId) xpPrim

-- ------------------------------------------------------------
