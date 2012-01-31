{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.LoadStore
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, UweSchmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  Load and store indexes

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.LoadStore
where

import Data.Binary      ( Binary (..) )
import qualified
       Data.Binary      as B

import qualified
       Data.List        as L

import Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | Try to determine the file type automatically. The file is loaded as XML if the filename
-- ends with \".xml\" and otherwise is loaded as binary file.

loadFromFile :: (XmlPickler a, Binary a) => FilePath -> IO a
loadFromFile f = if L.isSuffixOf ".xml" f then loadFromXmlFile f else loadFromBinFile f
 
-- | Load from XML file.

loadFromXmlFile :: XmlPickler a => FilePath -> IO a
loadFromXmlFile f = do
                    r <- runX (xunpickleDocument xpickle options f)
                    return $! head r
                    where
                    options = [ withRemoveWS yes, withInputEncoding utf8, withValidate no ]     

-- | Write to XML file.

writeToXmlFile :: XmlPickler a => FilePath -> a -> IO ()
writeToXmlFile f i = do
                     runX (constA i >>> xpickleDocument xpickle options f)
                      >> return ()
                     where
                     options = [ withIndent yes, withOutputEncoding utf8 ]

-- | Load from a binary file.

loadFromBinFile :: Binary a => FilePath -> IO a
loadFromBinFile = B.decodeFile

-- | Write to a binary file.

writeToBinFile :: Binary a => FilePath -> a -> IO ()
writeToBinFile = B.encodeFile

-- ------------------------------------------------------------
