module Main
where

import Text.XML.HXT.Arrow
import           Holumbus.Build.Crawl
import           Holumbus.Build.Index
import           Holumbus.Build.Tokenize
import           Holumbus.Index.Common
import qualified Holumbus.Index.Documents as DOC
import           Holumbus.Index.Inverted(emptyInverted)
import           Holumbus.Utility

import qualified Data.IntMap as IM
import Data.Maybe

main :: IO ()
main 
  = do
    traceLevel     <- return 0
    workerThreads  <- return 7
    idxConfig      <- return ic_fhw
    crawlState     <- return (initialCS idxConfig customFunction)

    -- ---------------------------------------------------------------------------------------------
    -- CRAWLING
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl traceLevel workerThreads crawlState

    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-docs.xml") (docs)
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") (docs)

    -- ---------------------------------------------------------------------------------------------
    -- INDEXING
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" indexing  ----------------------------- " ))

    localDocs <- return docs
    -- localDocs <- return $ tmpDocs (fromMaybe "/tmp" (ic_tmpPath idxConfig)) docs
    idx    <- buildIndex workerThreads
                         traceLevel
                         (map (\(i,d) -> (i, uri d)) (IM.toList $ DOC.toMap localDocs))
                         idxConfig
                         emptyInverted 

    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-index.xml") idx
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-index.bin") idx

    return()
    
customFunction :: ArrowXml a => a XmlTree (Maybe Int)
customFunction = constA Nothing    

ic_fhw :: IndexerConfig
ic_fhw 
  = IndexerConfig
    { ic_startPages     = [ "http://www.fh-wedel.de"
                          , "http://www.fh-wedel.de/sonstiges/sitemap/"
                          ]
    , ic_tmpPath        = Nothing -- Just "/tmp/"
    , ic_idxPath        = "/home/sms/indexes/sitemap"
    , ic_contextConfigs = ccs_fhw
    , ic_readAttrs      = stdOpts4Reading
    , ic_fCrawlFilter   = mkCrawlFilter [ "http://(www)?\\.fh-wedel\\.de" ]                 -- allow
                                        [ "tx_fhwunternehmensforum_pi3"                     -- deny
--                                        , "/~", "/%7E", "http://www.fh-wedel.de/mitarbeiter/"
                                        , "\\?L=0"
                                        , ".pdf$", ".jpg$", ".gif$", ".png$", ".tar.gz$"
                                        , ".ppt$", ".exe$", ".txt$", ".zip$", ".doc$"
                                        , ".dot$", ".png$", ".ps$", ".ps.gz$", ".nb$"
                                        , ".swf$", ".JPG$", ".tex$"
                                        , "%7Edi", "/~di"
                                        , "/rundgang/id=", "/vorlesungsplan/id="
                                        , "/vorlesungsplan/sem=", "/tv-infosystem/", "/~splan/"
                                        ]
    }
    
-- | The list of context configurations for the Fh Wedel pages
ccs_fhw :: [ContextConfig]
ccs_fhw = []
      ++ [cc_title]
      ++ [cc_meta]
      ++ [cc_content]
      ++ [cc_raw]

    
-- | Context for title-tags
cc_title :: ContextConfig
cc_title
  = ContextConfig
    { cc_name         = "title"
    , cc_preFilter    = this
    , cc_XPath        = "/html/head/title"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = const False -- (\s -> length s < 2)
    }
    
-- | Context for meta information. Description and keywords will be indexed
cc_meta :: ContextConfig
cc_meta
  = ContextConfig
    { cc_name         = "meta"
    , cc_preFilter    = this
    , cc_XPath        = "/html/head/meta[@name='description' or @name='keywords']/@content"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = (\s -> length s < 2)
    }    
    
-- | Context for normal page content. This indexes everything that is inside a div element with id
--   "col2_content" as it is defined in the central fhw template. Pages that do not use the normal
--   template will only be indexed in the raw context
cc_content :: ContextConfig
cc_content
  = ContextConfig
    { cc_name         = "content"
    , cc_preFilter    = this
    , cc_XPath        = "//div[@id='col2_content']"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = (\s -> length s < 2)
    }

-- | Raw context. This indexes all the text that is listed on the page without check if it is in
--   a menu or if it is real text. This context will have very low importance for search results
cc_raw :: ContextConfig
cc_raw
  = ContextConfig
    { cc_name         = "raw"
    , cc_preFilter    = this
    , cc_XPath        = "//body"  
    , cc_fTokenize    = map (stripWith (=='.')) . (parseWords isWordChar)
    , cc_fIsStopWord  = (\s -> length s < 2)
    }
    
        
    
-- | some standard options for the readDocument function
stdOpts4Reading :: [(String, String)]
stdOpts4Reading = []
  ++ [ (a_parse_html, v_1)]
  ++ [ (a_issue_warnings, v_0)]
  ++ [ (a_tagsoup, v_1) ]
  ++ [ (a_use_curl, v_1)]
  ++ [ (a_options_curl, "-L")] --"--user-agent HolumBot/0.1 --location")]     