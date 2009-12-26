-- ----------------------------------------------------------------------------

{- |
  Module     : Config
  Copyright  : Copyright (C) 2009 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: untested
  Version    : 0.1

  Config for the Hayoo Indexer 

-}

-- ----------------------------------------------------------------------------

module Hayoo.Config where


import           Hayoo.Common

import           Data.Char
import           Data.List
import           Data.Maybe

import           Holumbus.Build.Config
import           Holumbus.Build.Index

import           Holumbus.Index.Common
import           Holumbus.Utility

import           Text.XML.HXT.Arrow     


-- | config for hackage
ic_Hayoo :: IndexerConfig
ic_Hayoo
  = IndexerConfig
    { ic_startPages      = [ "http://hackage.haskell.org/packages/archive/pkg-list.html" ]
    , ic_tempPath        = Just "/tmp/holumbus/hayoo/"
    , ic_indexPath       = "/tmp/hayoo"  -- will be overwritten in main-function
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs  = ccs_Hayoo
    , ic_readAttributes  = standardReadDocumentAttributes
    , ic_fCrawlFilter    = hackageFilter
    }
    

-- | config for non-hackage documents, currently only gtk2hs
ic_Hayoo_additional :: IndexerConfig
ic_Hayoo_additional
  = ic_Hayoo
    { ic_startPages      = [ "http://www.haskell.org/gtk2hs/docs/current/"
                           ]
    , ic_fCrawlFilter    = simpleCrawlFilter ["http://www.haskell.org/gtk2hs/docs/current/"] []
    }   

-- | this config is used for debugging. it uses a small set of documents
ic_HTTP :: IndexerConfig
ic_HTTP 
  = IndexerConfig
    { ic_startPages     = [ "http://hackage.haskell.org/packages/archive/HTTP/latest/doc/html/Network-Browser.html"]
    , ic_tempPath        = Just "/tmp/"
    , ic_indexPath        = "/home/sms/indexes/http"
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs = ccs_Hayoo
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "http://hackage.haskell.org/packages/archive/HTTP/latest/doc/html/"]
                                            [ "/src/"]
    }  

-- | this config is used for debugging. it uses a small set of documents
ic_Holumbus :: IndexerConfig
ic_Holumbus
  = IndexerConfig
    { ic_startPages     = [ "http://hackage.haskell.org/packages/archive/Holumbus-MapReduce/latest/doc/html/Holumbus-Standalone-SMapReduce.html"]
    , ic_tempPath        = Just "/tmp/"
    , ic_indexPath        = "/home/sms/indexes/http"
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs = ccs_Hayoo
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "http://hackage.haskell.org/packages/archive/Holumbus-MapReduce/latest/doc/html/"]
                                            [ "/src/"]
    }  

-- -----------------------------------------------------------------------------    

hackageFilter :: URI -> Bool
hackageFilter = simpleCrawlFilter [ "http://hackage.haskell.org/" ]                  -- allow
                                         [ "/src/", "/trac/", ".tar.gz$", ".cabal$", ".pdf$"-- deny
                                         , "/doc-index-", "doc-index.html", "/logs/failure/" 
                                         , "http://hackage.haskell.org/cgi-bin/hackage-scripts/list-users"
                                         , "http://hackage.haskell.org/packages/hackage.html"
                                         , "Data-TypeLevel-Num-Aliases.html"
                                         , "Graphics-X11-Types.html"
                                         , "Harpy-X86Assembler.html"
                                         , "Database-HaskellDB-BoundedList.html"
                                         , "Graphics-UI-WXCore-WxcClassTypes.html"
                                         , "Graphics-UI-WXCore-WxcClassesAL.html"
                                         , "Graphics-UI-WXCore-WxcClassesMZ.html"
                                         , "Graphics-UI-WXCore-WxcDefs.html"
                                         , "Types-Data-Num-Decimal-Literals.html"
                                         ]
                                         
preCrawlFilter :: ArrowXml a => a XmlTree XmlTree -- LA XmlTree XmlTree
preCrawlFilter 
  = processTopDown (
      none
      `when`
      (    hasName "tr" 
        /> hasName "th" >>> hasAttrValue "class" (== "horizontal")
        /> hasText (\a -> a `elem` ["Other versions", "Dependencies"])
      )
    )

                        

theHayooXPath :: String
theHayooXPath =  "//td[@class='decl']"     
  
ccs_Hayoo :: [ContextConfig]
ccs_Hayoo = [  ccModule
            , ccHierarchy
            , ccHayooName     
            , ccHayooPartialName 
            , ccHayooSignature 
            , ccHayooNormalizedSignature
            , ccHayooDescription
            , ccPackage
            ]
            
ccHayooName :: ContextConfig
ccHayooName 
  = ContextConfig { cc_name        = "name"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees theHayooXPath  
                  , cc_fTokenize   = (\a -> [stripWith (\b -> b `elem` "():") (strip (takeWhile ( (/=) ' ') (dropWhile ( (==) ' ') a)))])
                  , cc_fIsStopWord = (flip elem) ["type", "class", "data"]  
                  , cc_addToCache  = False
                  }

ccHayooPartialName :: ContextConfig
ccHayooPartialName 
  = ccHayooName   { cc_name        = "partial"
                  , cc_fTokenize   = (\s -> split " " (deCamel False (takeWhile ( (/=) ' ') s))) 
                  } 
  where 
    deCamel :: Bool -> String -> String  -- Bool flag: last character was a capital letter
    deCamel _ []     = []
    deCamel wasCap (x:xs) 
      = if x == '_' then ' ' : deCamel True xs
                    else if x `elem` ['A'..'Z'] then if wasCap then x: deCamel True xs
                                                               else ' ' : (x : deCamel True xs) 
                                                else x : deCamel (x `elem` ['A'..'Z']) xs

-- | Configruation for description context.
ccHayooDescription :: ContextConfig 
ccHayooDescription 
  = ContextConfig { cc_name        = "description" 
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees "//td[@class='doc']"
                  , cc_fTokenize   = map (stripWith (=='.')) . 
                                         (parseWords (\c -> isAlphaNum c || c `elem` ".-_'@():"))
--                  , cc_fTokenize   = map (stripWith (=='.')) . (parseWords isWordChar)
                  , cc_fIsStopWord = \s -> length s < 2
                  , cc_addToCache  = True
                  }

ccHayooSignature :: ContextConfig
ccHayooSignature  
  = ContextConfig { cc_name        = "signature"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees theHayooXPath
                  , cc_fTokenize   = \s -> [getSignature s]
                  , cc_fIsStopWord = const False
                  , cc_addToCache  = False     
                  }

ccHayooNormalizedSignature :: ContextConfig
ccHayooNormalizedSignature 
  = ccHayooSignature { cc_name        = "normalized"
                     , cc_fTokenize   = (\s -> [normalizeSignature (getSignature s)])
                     , cc_fIsStopWord = (\s -> length s ==  0) 
                     }
                  
ccModule :: ContextConfig
ccModule 
  = ContextConfig { cc_name        = "module"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees "/table/tr/td[@id='module']" 
                  , cc_fTokenize   = \a -> [stripWith (==' ') a]
                  , cc_fIsStopWord = const False     
                  , cc_addToCache  = False
                  }

ccHierarchy :: ContextConfig
ccHierarchy 
  = ccModule      { cc_name        = "hierarchy" 
                  , cc_fTokenize   = split "." . stripWith (==' ') 
                  }     
            
ccPackage :: ContextConfig
ccPackage
  = ContextConfig { cc_name        = "package"
                  , cc_preFilter   = this
                  , cc_fExtract    = getXPathTrees "/table/tr/td[@id='package']"
                  , cc_fTokenize   = \a -> [stripWith (==' ') a]
                  , cc_fIsStopWord = (flip elem) ["unknownpackage"]
                  , cc_addToCache  = False
                  }
                              
getSignature :: String -> String
getSignature s = stripWith (==' ') $ 
                  if "=>" `isInfixOf` s  then stripSignature $ drop 3 $ dropWhile ((/=) '=') s
                                         else stripSignature $ drop 3 $ dropWhile ((/=) ':') s


-- | TODO some of this is redundant, some important. find out what and why and simplify
preFilterSignatures :: LA XmlTree XmlTree
preFilterSignatures = this
--      >>> topDeclToDecl
      >>> flattenElementsByType "a"
      >>> flattenElementsByType "b"
      >>> flattenElementsByType "span"
      >>> flattenElementsByType "em"
      >>> flattenElementsByType "p"
      >>> flattenElementsByType "pre" 
--      >>> removeDeclbut


flattenElementsByType :: String -> LA XmlTree XmlTree
flattenElementsByType s = processTopDown ( getChildren `when` hasName s )

removeDeclbut :: ArrowXml a => a XmlTree XmlTree
removeDeclbut = processTopDown (  none `when` isDeclbut )
  where isDeclbut = isElem  >>> hasName "td" >>> hasAttrValue "class" (isPrefixOf "declbut") 

