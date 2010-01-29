-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Build.Crawl
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  Includes data types for the configuration of crawlers and indexers as well as 
  helper functions to create configuriations. 

-}

-- -----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
-- -----------------------------------------------------------------------------

module Holumbus.Build.Config 
  (
  -- * Basic data types
    IndexerConfig(..)
  , ContextConfig(..)
  , CrawlerState(..)
  , Custom
  
  , initialCrawlerState
  , loadCrawlerState
  , saveCrawlerState
  
  , mergeIndexerConfigs
  , mergeIndexerConfigs'
  
  -- * Crawler configuration helpers
  , getReferencesByXPaths
  , getHtmlReferencesByXPaths
  , getHtmlReferences
  , crawlFilter
  , simpleCrawlFilter
  , standardReadDocumentAttributes
  , standardReadTmpDocumentAttributes
  , standardWriteTmpDocumentAttributes
  
  -- * Tokenizing
  , parseWords
  , isWordChar
  )

where

import           Data.Binary
import           Data.Char
import           Data.List
import           Data.Maybe

import qualified Data.Map       as M
import qualified Data.Set       as S

-- import qualified Data.IntMap as IM


import           Holumbus.Index.Common
-- import           Holumbus.Index.Documents
import           Holumbus.Utility

import           Text.Regex

import           Text.XML.HXT.Arrow
import           Text.XML.HXT.XPath

{-
import qualified Debug.Trace as D

-- ------------------------------------------------------------

dbg             :: Show a => a -> a
dbg x           = D.trace ("debug " ++ show x) x
-}
-- ------------------------------------------------------------

type Custom a = IOSArrow XmlTree (Maybe a)
type MD5Hash = String


-- | Configuration for the indexer. 

data IndexerConfig 
  = IndexerConfig
    { ic_startPages     :: [URI]
    , ic_tempPath       :: Maybe String   
    , ic_indexPath      :: String
    , ic_contextConfigs :: [ContextConfig]
    , ic_fCrawlFilter   :: URI -> Bool     -- will be passed to crawler, not needed for indexing
    , ic_readAttributes :: Attributes
    , ic_indexerTimeOut :: Int
    } 
   
-- | Configuration for a Context. It has a name with which it will be identified
--   as an index part. The preFilter is applied to the XmlTree that is generated
--   by the parser and before the "interesting" document parts are selected by
--   the XPath Expression. The Tokenize functions defines how a string found by
--   the XPath Expression will be split into a list of Words. Since stopwords
--   are identified by a function it is possible to define a list of words that
--   shall not be indexed or a function that excludes words because of the count
--   of characters or a combination of both.

data ContextConfig 
  = ContextConfig
    { cc_name           :: String
    , cc_preFilter      :: ArrowXml a => a XmlTree XmlTree
    , cc_fExtract       :: ArrowXml a => a XmlTree XmlTree             
    , cc_fTokenize      :: String -> [String]
    , cc_fIsStopWord    :: String -> Bool
    , cc_addToCache     :: Bool
    }   
    
-- | crawler state

data CrawlerState d a
    = CrawlerState
      { cs_toBeProcessed    :: S.Set URI
      , cs_wereProcessed    :: S.Set URI
      , cs_docHashes        :: Maybe (M.Map MD5Hash URI)
      , cs_nextDocId        :: DocId  
      , cs_readAttributes   :: Attributes     -- passed to readDocument
      , cs_tempPath         :: Maybe String     
      , cs_crawlerTimeOut   :: Int
      , cs_fPreFilter       :: ArrowXml a' => a' XmlTree XmlTree  -- applied before link extraction
      , cs_fGetReferences   :: ArrowXml a' => a' XmlTree [URI]
      , cs_fCrawlFilter     :: (URI -> Bool)			  -- decides if a link will be followed
      , cs_fGetCustom       :: Custom a
      , cs_docs             :: HolDocuments d a => d a       
      }    
    
instance (HolDocuments d a, Binary a) => Binary (CrawlerState d a) where
  put (CrawlerState tbp wp dh ndi _ _ cto _ _ _ _ d) 
    = put tbp >> put wp >> put dh >> put ndi >> put cto >> put d
      
  get = do
        tbp <- get
        wp  <- get
        dh  <- get
        ndi <- get
        cto <- get
        d   <- get
        return $ CrawlerState { cs_toBeProcessed  = tbp 
                              , cs_wereProcessed  = wp
                              , cs_docHashes      = dh 
                              , cs_nextDocId      = ndi
                              , cs_readAttributes = [] 
                              , cs_tempPath       = Nothing
                              , cs_crawlerTimeOut = cto
                              , cs_fPreFilter     = this
                              , cs_fGetReferences = constA []
                              , cs_fCrawlFilter   = const False
                              , cs_fGetCustom     = constA Nothing
                              , cs_docs           = d
                              } 
         
-- | create an initial CrawlerState from an IndexerConfig

initialCrawlerState :: (HolDocuments d a, Binary a) => IndexerConfig -> d a -> Custom a -> CrawlerState d a
initialCrawlerState cic emptyDocuments getCustom
  = CrawlerState
    { cs_toBeProcessed  = S.fromList (ic_startPages cic)
    , cs_wereProcessed  = S.empty
    , cs_nextDocId      = 1
    , cs_readAttributes = ic_readAttributes cic
    , cs_crawlerTimeOut = ic_indexerTimeOut cic
    , cs_fGetReferences = getHtmlReferences    -- or getHtmlReferencesByXPaths
    , cs_fPreFilter     = (none `when` isText) -- this
    , cs_fCrawlFilter   = ic_fCrawlFilter cic
    , cs_docs           = emptyDocuments
    , cs_tempPath       = ic_tempPath cic
    , cs_fGetCustom     = getCustom
    , cs_docHashes      = Just $ M.empty
    }
   
    
-- | write CrawlerState to file

saveCrawlerState :: (HolDocuments d a, Binary a) => FilePath -> CrawlerState d a -> IO ()
saveCrawlerState fp cs = writeToBinFile fp cs

-- | load CrawlerState from file. Since the functions can not be written 
--   they have to be taken from another CrawlerState that has to be supplied
--   as second parameter.

loadCrawlerState :: (HolDocuments d a , Binary a) => FilePath -> CrawlerState d a -> IO (CrawlerState d a)
loadCrawlerState fp ori = do
                          cs <- decodeFile fp
                          return $! cs { cs_readAttributes = cs_readAttributes ori
                                    , cs_fPreFilter     = cs_fPreFilter     ori
                                    , cs_fGetReferences = cs_fGetReferences ori
                                    , cs_tempPath       = cs_tempPath       ori
                                    , cs_fGetCustom     = cs_fGetCustom     ori
                                    }    
                                    


-- | Merge Indexer Configs. Basically the first IndexerConfig is taken and
--   the startPages of all other Configs are added. The crawl filters are OR-ed
--   so that more pages might be indexed. So you better know what you are doing
--   when you are using this.

mergeIndexerConfigs :: IndexerConfig -> IndexerConfig -> IndexerConfig
mergeIndexerConfigs cfg1 cfg2 = mergeIndexerConfigs' cfg1 [cfg2]

mergeIndexerConfigs' :: IndexerConfig -> [IndexerConfig] -> IndexerConfig
mergeIndexerConfigs' cfg1 [] = cfg1
mergeIndexerConfigs' cfg1 (cfg2:cfgs) = mergeIndexerConfigs' resCfg cfgs
  where 
  resCfg = cfg1 { ic_startPages   = (ic_startPages cfg1) ++ (ic_startPages cfg2)
                , ic_fCrawlFilter = (\a -> (ic_fCrawlFilter cfg1) a || (ic_fCrawlFilter cfg2) a)
                }

{- | Create Crawl filters based on regular expressions. The first Parameter defines the default 
     value if none of the supplied rules matches. The rule list is computed from the first element
     to the last. The first rule that matches the URI is applied. 
     
     example:
     
     > crawlFilter False [ ("\/a\/b\/z", True )
     >                   , ("\/a\/b"  , False)
     >                   , ("\/a"    , True )

     The default value for the filter is False like it will be in most cases unless you are trying
     to use Holumbus to build a google replacement. If you read the rules from bottom to top, all
     documents in "\/a" will be included (which should be a single domain or ip address or maybe a
     set of these). The second rule disables the directory "\/a\/b" but with the first rule, the 
     subdirectory z is included again and "\/a\/b\/a" to "\/a\/b\/y" are excluded. Even though this could
     also be done with the 'simpleCrawlFilter', this way saves you a lot of unnecessary code.
-} 

crawlFilter :: Bool -> [(String, Bool)] -> (URI -> Bool)
crawlFilter theDefault [] _ = theDefault
crawlFilter theDefault ((expr, b):expressions) theUri = 
  if isJust $ matchRegex (mkRegex expr) theUri then b else crawlFilter theDefault expressions theUri


-- | Create Crawl filters based on regular expressions. The first list defines 
--   regular expression of URIs that have to be crawled. Any new URI must match at least one of 
--   these regular expressions to be crawled. The second list consists of regular expressions for
--   pages that must not be crawled. This can be used to limit the set of documents defined by 
--   the including expressions. 

simpleCrawlFilter :: [String] -> [String] -> (URI -> Bool)
simpleCrawlFilter as ds theUri = isAllowed && (not isForbidden ) 
         where
         isAllowed   = foldl (||) False (map (matches theUri) as)
         isForbidden = foldl (||) False (map (matches theUri) ds)
         matches u a = isJust $ matchRegex (mkRegex a) u      

-- ------------------------------------------------------------


-- | Extract references to other documents from a XmlTree based on configured XPath expressions

getReferencesByXPaths :: ArrowXml a => [String] -> a XmlTree [URI]
getReferencesByXPaths xpaths
  = listA (getRefs' $< computeDocBase) -- >>^ concat
    where
    getRefs' base = catA $ map (\x -> getXPathTrees x >>> getText >>^ toAbsRef base) xpaths

getHtmlReferencesByXPaths :: ArrowXml a => a XmlTree [URI]
getHtmlReferencesByXPaths
    = getReferencesByXPaths [ "//a/@href/text()"
                            , "//frame/@src/text()"
                            , "//iframe/@src/text()"
                            ]

getHtmlReferences :: ArrowXml a => a XmlTree [URI]
getHtmlReferences
    = listA (getRefs' $< computeDocBase)
    where
    getRefs' base
        = fromLA $
          deep (hasNameWith ((`elem` ["a","frame","iframe"]) . localPart))
          >>>
          ( getAttrValue0 "href"
            <+>
            getAttrValue0 "src"
          )
          >>^ (toAbsRef base)

toAbsRef        :: URI -> URI -> URI
toAbsRef base ref
    = removeFragment . fromMaybe ref . expandURIString ref $ base
    where
    removeFragment r
        | "#" `isPrefixOf` path = reverse . tail $ path
        | otherwise 		= r
        where
        path = dropWhile (/='#') . reverse $ r 

          
-- ------------------------------------------------------------

-- | Split a string into a list of words.

parseWords  :: (Char -> Bool) -> String -> [String]
parseWords isWordChar'
          = filter (not . null) . words . map boringChar
          where
          boringChar c             -- these chars separate words
            | isWordChar' c = c
            | otherwise     = ' '

-- | standard function to identify non-separating characters for words

isWordChar  :: Char -> Bool
isWordChar c = isAlphaNum c || c `elem` ".-_'@" 

-- ------------------------------------------------------------
     
-- | some standard options for the readDocument function

standardReadDocumentAttributes :: [(String, String)]
standardReadDocumentAttributes
    = [ (a_parse_html,               v_1)
      , (a_encoding,                 isoLatin1)
      , (a_issue_warnings,           v_0)
      , (a_remove_whitespace,        v_1)
      , (a_tagsoup,                  v_1)
      , (a_parse_by_mimetype,        v_1)
      , (a_ignore_none_xml_contents, v_1)
      , (a_use_curl,                 v_1)       -- obsolete since hxt-8.1, 
      , ("curl--user-agent",         "HolumBot/0.1@http://holumbus.fh-wedel.de --location")
      ]

-- | options for writing the tmp files

standardWriteTmpDocumentAttributes :: [(String, String)]
standardWriteTmpDocumentAttributes
    = [ (a_indent,                      v_1)    -- for testing only, should be v_0 for efficiency
      , (a_remove_whitespace,           v_0)
      , (a_output_encoding,             utf8)
      ]

-- | options for reading the tmp files

standardReadTmpDocumentAttributes :: [(String, String)]
standardReadTmpDocumentAttributes
    = [ (a_indent,                      v_0)
      , (a_remove_whitespace,           v_0)
      , (a_encoding,                    utf8)
      , (a_parse_html,                  v_0)    -- force XML parsing
      , (a_parse_by_mimetype,           v_0)
      , (a_validate,                    v_0)
      , (a_canonicalize,                v_0)
      , (a_encoding,                    utf8)   -- must correspond to defaultTmpWriteDocumentAttributes
      ]

-- ------------------------------------------------------------
