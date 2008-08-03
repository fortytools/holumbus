{-# OPTIONS_GHC -XFlexibleInstances #-}

module Main
where

import           Text.XML.HXT.Arrow
import           Holumbus.Build.Config
import           Holumbus.Build.Crawl
import           Holumbus.Build.Index
import           Holumbus.Index.Common
import           Holumbus.Index.Documents

import qualified Data.IntMap as IM
import qualified Data.Map as M

import           Data.Maybe

import           System.Directory


instance XmlPickler (Documents String) where
  xpickle = xpDocuments 

instance XmlPickler (Document String) where
  xpickle = xpDocument    

xpDocuments :: PU (Documents String)
xpDocuments = xpElem "urlset" $ xpWrap convertDoctable (xpWrap (IM.fromList, IM.toList) (xpList xpUrl))
    where
    convertDoctable = (\itd -> Documents itd (idToDoc2docToId itd) (lastId itd), \(Documents itd _ _) -> itd)
    xpUrl           = xpElem "url" (xpPair xpZero xpDocument)

xpDocument :: PU (Document String)
xpDocument = xpWrap ( \(t, u, i) -> Document t u i
                    , \(Document t u i) -> (t, u, i)
                    ) (xpTriple xpTitle xpURI xpModified)
    where
    xpURI           = xpAttr "href" xpText0
    xpTitle         = xpAttr "title" xpText0
    xpModified      = xpOption $ xpAttr "lastmod" xpText

main :: IO ()
main 
  = do
    dir <- getHomeDirectory
    let traceLevel    = 1
        workerThreads = 1
        docsPerCrawl  = 0
        indexPath     = dir ++ "/indexes/sitemap"
        idxConfig     = ic_haskellwiki {ic_indexPath = indexPath}
        crawlerState  = initialCrawlerState idxConfig emptyDocuments customFunction 
    
    createDirectoryIfMissing True (dir ++ "/indexes/")
    
    -- ---------------------------------------------------------------------------------------------
    -- CRAWLING
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl traceLevel workerThreads docsPerCrawl crawlerState 

    writeToXmlFile' ( (ic_indexPath idxConfig) ++ "-docs.xml") (docs)
    writeToBinFile  ( (ic_indexPath idxConfig) ++ "-docs.bin") (docs)

    return()
    
customFunction :: ArrowXml a => a XmlTree (Maybe String)
customFunction = 
  fromLA (     getXPathTrees "//div[@class='printfooter']/p" >>> getTexts >>> getText
           >>^ \s -> Just $ (takeWhile (/='.') . drop 4 . dropWhile (/= ':')) s
         )        
                     

ic_haskellwiki :: IndexerConfig
ic_haskellwiki
  = IndexerConfig 
    { ic_startPages     = [ "http://haskell.org"
                          , "http://haskell.org/haskellwiki/Special:Allpages/5_ad%C4%B1mda_Haskell"
                          , "http://haskell.org/haskellwiki/?title=Special:Allpages&from=Introduction_to_IO"
                          ]
    , ic_indexerTimeOut = 120 * 1000000 -- two minutes
    , ic_tempPath       = Nothing
    , ic_indexPath      = "/tmp/" -- will be replaced
    , ic_contextConfigs = []
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "http://(www)?\\.haskell\\.org" ]             -- allow
                                            [ "/pipermail/"                                 -- deny
                                            , "/docs/"  -- don't mess with hayoo
                                            , "&oldid=" -- no old revisions
                                            , "action=history"
                                            , "action=edit"
                                            ]
    }
        
-- | Write to XML file.
writeToXmlFile' :: FilePath -> Documents String -> IO ()
writeToXmlFile' f d = runX (constA d >>> xpickleDocument xpDocuments options f)
                      >> return ()
                      where
                      options = [ (a_indent, v_1), (a_output_encoding, utf8), (a_validate, v_0) ]         
        
-- | Construct the inverse map from the original map.
idToDoc2docToId :: IM.IntMap (Document a) -> M.Map URI DocId
idToDoc2docToId = IM.foldWithKey (\i d r -> M.insert (uri d) i r) M.empty

-- | Query the 'idToDoc' part of the document table for the last id.
lastId :: IM.IntMap (Document a) -> Int
lastId = (IM.foldWithKey (\k _ r -> max k r) 0)        