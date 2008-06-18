-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Build.Crawl
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  Sample Configurations for Indexer Applications. This will later change to be
  the interface for configuring indexers by xml files.

-}

-- -----------------------------------------------------------------------------

module Holumbus.Build.Config where

import           Data.Char
import           Data.List

import           Holumbus.Build.Index

-- import           Holumbus.Index.Common

import           Text.XML.HXT.Arrow



parseWords  :: (Char -> Bool) -> String -> [String]
parseWords isWordChar'
          = filter (not . null) . words . map boringChar
          where
          boringChar c             -- these chars separate words
            | isWordChar' c = c
            | otherwise    = ' '

isWordChar  :: Char -> Bool
isWordChar c = isAlphaNum c || c `elem` ".-_'@" 


instance XmlPickler IndexerConfig where
  xpickle = xpWrap  ( \(sp, tp, ip, cc, cf, ra) -> IndexerConfig sp tp ip cc cf ra
                    , \(IndexerConfig sp tp ip cc cf ra) -> (sp, tp, ip, cc, cf, ra)
                    ) xpConfig
    where
    xpConfig = xp6Tuple xpStartPages xpTmpPath xpIdxPath xpContextConfigs xpFCrawlFilter xpReadAttrs
      where -- We are inside a doc-element, therefore everything is stored as attribute.
      xpStartPages     = xpElem "StartPages" $ xpList   $ xpElem "Page"       xpPrim 
      xpTmpPath        = xpOption $ xpElem "TmpPath"    xpPrim
      xpIdxPath        =            xpElem "OutputPath" xpPrim
      xpContextConfigs = xpElem "ContextConfigurations" $ xpList $ xpContextConfig
      xpContextConfig  = xpZero
      xpFCrawlFilter   = xpZero
      xpReadAttrs      = xpZero
      
--data IndexerConfig 
--  = IndexerConfig
--    { ic_startPages     :: [URI]
--    , ic_tmpPath        :: Maybe String   
--    , ic_idxPath        :: String
--    , ic_contextConfigs :: [ContextConfig]
--    , ic_fCrawlFilter   :: URI -> Bool     
--    , ic_readAttrs      :: Attributes
--    } 
         
  






