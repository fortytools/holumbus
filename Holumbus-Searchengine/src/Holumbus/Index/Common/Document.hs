{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.Document
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The Document datatype

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.Document
where

import           Control.DeepSeq
import           Control.Monad                    (liftM3)

import           Data.Binary                      (Binary (..))
import           Data.Size
import           Data.Typeable

import           Holumbus.Index.Common.BasicTypes

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | A document consists of a title and its unique identifier (URI)
-- and a customizable component

data Document a                 = Document
                                  { title  :: ! Title
                                  , uri    :: ! URI
                                  , custom :: ! (Maybe a)
                                  }
                                  deriving (Show, Eq, Ord, Typeable)

instance Binary a => Binary (Document a) where
    put (Document t u c)        = put t >> put u >> put c
    get                         = liftM3 Document get get get

instance XmlPickler a => XmlPickler (Document a) where
    xpickle                     = xpWrap ( \ (t, u, i) -> Document t u i
                                         , \ (Document t u i) -> (t, u, i)
                                         ) (xpTriple xpTitle xpURI xpickle)
        where
        xpURI                   = xpAttr "href"  xpText0
        xpTitle                 = xpAttr "title" xpText0

instance NFData a => NFData (Document a) where
    rnf (Document t u c)        = rnf t `seq` rnf u `seq` rnf c

instance Sizeable a => Sizeable (Document a) where
    dataOf _x                   = 3 .*. dataOfPtr
    statsOf x@(Document t u c)  = mkStats x <> statsOf t <> statsOf u <> statsOf c

-- ------------------------------------------------------------
