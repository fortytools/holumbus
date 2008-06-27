module Main
where


import           Text.XML.HXT.Arrow
import           Holumbus.Build.Config
import           Holumbus.Build.Crawl
import           Holumbus.Build.Index

import           Holumbus.Index.Common
import           Holumbus.Index.Documents (Documents, emptyDocuments, toMap)
import           Holumbus.Utility

import           Holumbus.Control.MapReduce.Parallel

import           Control.Monad

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

import           Data.Binary
import           Data.List
import           Data.Maybe




-- | The naive data for the PageRank Calculation
data PRInfo = PRInfo 
  { refsFrom   :: [URI]         -- ^ Outbound links
--  , refsTo     :: [URI]         -- ^ Inbound links
  , linkCount  :: Int           -- ^ Count of outbound links
  , pageRank   :: Float
  } 
  deriving (Show, Eq)

instance Binary PRInfo where
  put (PRInfo r c p) = put r >> put c >> put p
  get = liftM3 PRInfo get get get



instance XmlPickler PRInfo where
  xpickle = xpWrap (\(r, c, p) -> PRInfo r c p, \(PRInfo r c p) -> (r, c, p)) xpInfo
    where
    xpInfo = xpTriple xpRefs xpCount xpPageRank
      where -- We are inside a doc-element, therefore everything is stored as attribute.
      xpRefs = xpElem "refs" (xpList $ xpElem "ref" xpText0)
      xpCount = xpAttr "refcount" xpPrim
      xpPageRank = xpAttr "pagerank" xpPrim      



main :: IO ()
main 
  = do
    -- ---------------------------------------------------------------------------------------------
    -- CONFIGURATION
    -- ---------------------------------------------------------------------------------------------
    let traceLevel     = 0
        workerThreads  = 7
        theD           = 0.5
        iterations     = 20
        docsPerCrawl   = 2000000
        idxConfig      = ic_si
--        idxConfig      = ic_fhw

        crawlerState   = initialCrawlerState idxConfig emptyDocuments (customFunction (ic_fCrawlFilter idxConfig))
 
    -- ---------------------------------------------------------------------------------------------
    -- CRAWLING
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl traceLevel workerThreads docsPerCrawl crawlerState

    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-pre-docs.xml") (docs)
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-pre-docs.bin") (docs)
    
    -- ---------------------------------------------------------------------------------------------
    -- PageRank calculation
    -- ---------------------------------------------------------------------------------------------
    -- docs <- loadFromXmlFile ((ic_idxPath idxConfig) ++ "-docs.xml") :: IO (Documents PRInfo)
    
    -- looping haskellish
    rankedDocs <- foldM (\d _ -> calculatePageRank theD d) docs [1.. iterations]

    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-docs.xml") (rankedDocs)
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") (rankedDocs)
    
    return()

calculatePageRank :: Float -> Documents PRInfo -> IO (Documents PRInfo)
calculatePageRank theD docs
  = do
    let docs' = IM.toList $ toMap docs
    mr      <- mapReduce 7 mAP (rEDUCE docs') docs'
    newDocs <- return $ map (calculate mr) (IM.toList $ toMap docs)
    return $! foldl' (\d r -> snd (insertDoc d r)) emptyDocuments newDocs
    where
      calculate :: (M.Map URI Float) -> (Int, Document PRInfo) -> Document PRInfo
      calculate m (_, d) = let c = fromJust (custom d) in
                            d {custom = Just $ PRInfo (refsFrom c) (linkCount c) ((fromJust (M.lookup (uri d) m)) * theD + (1-theD) ) }
      mAP :: DocId -> (Document PRInfo) -> IO[(URI, Float)]
      mAP _ doc = return $ map (\d -> (d, (pageRank c) / fromIntegral (linkCount c))) (refsFrom c)
        where
          c = fromJust (custom doc)
      rEDUCE :: [(DocId, Document PRInfo)] -> URI -> [Float] -> IO (Maybe Float)
      rEDUCE allDocs u pr = if S.member u docSet
                              then return $ Just $ foldl' (+) 0 pr
                              else return $ Nothing 
        where
          docSet = S.fromList (map (\(_, d) -> uri d) allDocs)
    
customFunction :: ArrowXml a => (String -> Bool) -> a XmlTree (Maybe PRInfo)
customFunction cf = custom' $< computeDocBase
  where
      custom' base =     listA (getXPathTrees "//a/@href/text()" >>> getText >>> arr toAbsRef )
                     >>> strictA
                     >>> arr (filter cf)
                     >>> mkPRInfo $<<<
                                        (      this
                                           &&& arr length
                                           &&& constA 0
                                        )
         where toAbsRef ref
                = removeFragment $ fromMaybe ref $ expandURIString ref base
                  where
                  removeFragment r
                    | "#" `isPrefixOf` path = reverse . tail $ path
                    | otherwise = r
                    where
                      path = dropWhile (/='#') . reverse $ r 

      mkPRInfo :: ArrowXml a => [URI] -> Int -> Float -> a b (Maybe PRInfo)
      mkPRInfo r c p = constA $ Just $ PRInfo r c p
       
ic_si :: IndexerConfig
ic_si = ic_fhw 
        { ic_startPages   = [ "http://www.fh-wedel.de/~si/"]
        , ic_fCrawlFilter = simpleCrawlFilter [ "http://(www)?\\.fh-wedel\\.de/~si/" ]
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
        

ic_fhw :: IndexerConfig
ic_fhw 
  = IndexerConfig
    { ic_startPages     = [ "http://www.fh-wedel.de"
                          , "http://www.fh-wedel.de/sonstiges/sitemap/"
                          ]
    , ic_tmpPath        = Just "/tmp/"
    , ic_idxPath        = "/home/sms/indexes/pagerank"
    , ic_contextConfigs = ccs_fhw
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "http://(www)?\\.fh-wedel\\.de" ]                 -- allow
                                        [ "tx_fhwunternehmensforum_pi3"                     -- deny
                                        , "/~", "/%7E", "http://www.fh-wedel.de/mitarbeiter/"
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
    , cc_addToCache   = False
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
    , cc_addToCache   = False
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
    , cc_addToCache   = False
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
    , cc_addToCache   = False
    }
    
        
  
