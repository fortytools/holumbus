-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Build.XmlFilter
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module provides functions to manipulate XmlTrees while building an index

-}

-- ----------------------------------------------------------------------------

module Holumbus.Build.XmlFilter where

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML

flattenAllElements :: ArrowXml a => a XmlTree XmlTree
flattenAllElements
  = processTopDown
      ( getChildren
        `when`
        isElem
      ) 

flattenElementsByType :: ArrowXml a => String -> a XmlTree XmlTree
flattenElementsByType elemName
  = processTopDown
      ( getChildren
        `when`
        ( isElem >>> hasName elemName )
      )
        
-- flattenElementsByXpath :: ArrowXml a => String -> a XmlTree XmlTree
-- flattenElementsByXpath _ = this -- processXPathTreesss Text.XML.HXT.Arrow.XmlNodeSet
      
-- removeElementsByXPath :: ArrowXml a => String -> a XmlTree XmlTree
-- removeElementsByXPath s = processXPathTrees none s        
        
        
        