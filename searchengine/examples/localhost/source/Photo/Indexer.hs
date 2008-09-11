module Main
where

import           Control.Monad hiding (when)

import           Text.XML.HXT.Arrow
import           Text.XML.HXT.RelaxNG

import           Holumbus.Build.Config
import           Holumbus.Build.Crawl
import           Holumbus.Build.Index
import           Holumbus.Index.Common
import           Holumbus.Index.Documents 
import           Holumbus.Index.Inverted.Memory(emptyInverted)
import           Holumbus.Utility

import qualified Data.IntMap as IM
import qualified Data.Set    as S
import           Data.Maybe
import           Data.Binary
import           Data.List

-- ------------------------------------------------------------

trc	:: String -> IO ()
trc m	= do
	  runX (traceMsg 0 m)
	  return ()

-- ------------------------------------------------------------

main :: IO ()
main 
  = do
    let traceLevel     = 1
        workerThreads  = 1
        docsPerCrawl   = 250
        docsPerIndex   = 2500
        idxConfig      = ic_photos
        crawlerState   = ( initialCrawlerState idxConfig emptyDocuments customFunction )
			 { cs_fGetReferences = fromLA editJSUris
			                       >>>
			                       getReferencesByXPaths ["//a/@href/text()"]
			 }

    trc " crawling  ----------------------------- "

    docs      <- crawl traceLevel workerThreads docsPerCrawl crawlerState
    localDocs <- return $ tmpDocs (fromMaybe "/tmp" (ic_tmpPath idxConfig)) docs

    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-docs.xml") (docs)
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") (docs)
        
    trc " indexing  ----------------------------- "

    pathes    <- buildSplitIndex 
		 workerThreads
                 traceLevel
                 localDocs
                 idxConfig
                 emptyInverted
                 True
                 docsPerIndex
    idx       <- foldM mergeIndexes' emptyInverted pathes

    
    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-index.xml") idx
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-index.bin") idx    
  
    return()
    where
      mergeIndexes' i1 f
	  = do
            i2 <- loadFromBinFile (f  ++ "-index.bin")
            return $ mergeIndexes i1 i2
                           
fromDocuments :: Binary a => CrawlerState Documents a -> Documents a -> CrawlerState Documents a
fromDocuments cs ds
    = cs { cs_toBeProcessed = S.fromList ( map (uri . snd) ( IM.toList $ toMap ds )) }
     
customFunction :: ArrowXml a => a XmlTree (Maybe Int)
customFunction = constA Nothing    


ic_photos :: IndexerConfig
ic_photos 
  = IndexerConfig
    { ic_startPages     = [ -- "http://192.168.2.11/~uwe/Alben/1600x1200-css/Photos/2007/2007.03.11.Hagenbeck/Gefluegel/pic-0005.html"
			    "http://192.168.2.11/~uwe/Alben/1600x1200-css/Photos.html"
                          ]
    , ic_contextConfigs = ccs_photo
    , ic_fCrawlFilter   = crawlFilter
                          False
                          [ -- ("^http://192.168.2.11/~uwe/Alben/1600x1200-css/Photos/2007/.*[.]html$", True)
			    ("^http://192.168.2.11/~uwe/Alben/1600x1200-css/Photos/.*[.]html$", True)
			  ]
    , ic_tmpPath        = Just "./pages/"	-- dir for crawled pages
    , ic_idxPath        = "./photo/hol"		-- dir and prefix for index files
    , ic_readAttributes = addEntries [ (a_remove_whitespace, v_1)
				     , (a_options_curl, "--user-agent HolumBot/0.5@http://hobel.welt.all")
				     , (a_canonicalize, v_1)
				     ]
                                     standardReadDocumentAttributes
    }
  
-- | The list of context configurations for the Fh Wedel pages
ccs_photo :: [ContextConfig]
ccs_photo
    = [ cc_title
      , cc_subtitle
      , cc_descr
      , cc_date
      , cc_tech
      ]

-- | Context for title-tags
cc_title :: ContextConfig
cc_title
  = ContextConfig
    { cc_name         = "title"
    , cc_preFilter    = this
    , cc_XPath        = "/html/head/title"  
    , cc_fTokenize    = tokenize [word2, ddmmyyyy]
    , cc_fIsStopWord  = const False
    , cc_addToCache   = False
    }

-- | Context for picture subtitles
cc_subtitle :: ContextConfig
cc_subtitle
  = ContextConfig
    { cc_name         = "subtitle"
    , cc_preFilter    = this
    , cc_XPath        = "/html/body/table/tr/td[@class=\"head2\"]/div[@class=\"subtitle\"]"  
    , cc_fTokenize    = tokenize [word2, ddmmyyyy]
    , cc_fIsStopWord  = const False
    , cc_addToCache   = False
    }
    
-- | Context for meta information. Description and keywords will be indexed
cc_descr :: ContextConfig
cc_descr
  = ContextConfig
    { cc_name         = "descr"
    , cc_preFilter    = fromLA $
                        selectInfoPart ( `elem`
					 [ "Titel"
					 , "Untertitel"
					 , "Beschreibung"
					 , "Kommentar"
					 , "Fotograf"
					 , "Autor"
					 , "Designer"
					 ] )
    , cc_XPath        = "/div"
    , cc_fTokenize    = tokenize [word2, ddmmyyyy]
    , cc_fIsStopWord  = const False
    , cc_addToCache   = False
    }    

-- | Context for date and time
cc_date :: ContextConfig
cc_date
  = ContextConfig
    { cc_name         = "date"
    , cc_preFilter    = fromLA $ selectInfoPart (== "Datum")
    , cc_XPath        = "/dic"
    , cc_fTokenize    = tokenize [yyyymmdd]
    , cc_fIsStopWord  = const False
    , cc_addToCache   = False
    }    

-- | Context for technical studff
cc_tech :: ContextConfig
cc_tech
  = ContextConfig
    { cc_name         = "tech"
    , cc_preFilter    = fromLA $
                        selectInfoPart ( `elem`
					 [ "Belichtungszeit"
					 , "Blende"
					 , "Brennweite"
					 , "Brennweite in 35mm"
					 , "Aufnahmebetriebsart"
					 , "Belichtung"
					 , "Belichtungskorrektur"
					 , "Belichtungsmessung"
					 , "ISO Empfindlichkeit"
					 , "Aufnamequalit\228t"
					 , "Farbraum"
					 , "Objektiv"
					 , "Objektiv (Modell)"
					 , "Scharfeinstellung"
					 , "Aufnahmemodus"
					 , "Wei\223abgleich"
					 , "Blitz"
					 , "Blitzeinstellung"
					 , "Scharfeinstellung"
					 , "Wei\223abgleich"
					 , "Kamera"
					 ] )
    , cc_XPath        = "/div"
    , cc_fTokenize    = tokenize [word]
    , cc_fIsStopWord  = const False
    , cc_addToCache   = False
    }    

-- ------------------------------------------------------------
--
-- word specs

word2		:: String
word2 		= "(\\p{L}+){2,}"				-- letter sequences with length >= 2 are words

yyyymmdd	:: String
yyyymmdd	= "[0-9]{4}-[0-9]{2}-[0-9]{2}"			-- date format, e.g 2008-07-03

ddmmyyyy	:: String
ddmmyyyy	= "[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{2,4}"	-- date format 29.2.2999

word		:: String
word		= "[^ \t\n\r]{2,}"

tokenize	:: [String] -> (String -> [String])
tokenize rs	= fromJust . tokenizeRE (intercalate "|" . map ((++ ")") . (++ "(")) $ rs)

-- ------------------------------------------------------------

editJSUris	:: LA XmlTree XmlTree
editJSUris
    = processTopDown ( processAttrl ( changeAttrValue editJSref
				      `when` hasName "href"
				    )
		       `when`
		       hasName "a"
		     )
    where
    editJSref ref
	| childRef `isPrefixOf` ref
	    = pord 3 . drop (length childRef) $ ref
	| "javascript:" `isPrefixOf` ref
	    = ""
	| otherwise
	    = ref
	where
	childRef = "javascript:childPage('"
	pord i = reverse . drop i . reverse

-- ------------------------------------------------------------

selectInfoPart	:: (String -> Bool) -> LA XmlTree XmlTree
selectInfoPart hasPart
    = replaceChildren $
      selem "div"
      [ deep ( ( hasName "table" >>> hasAttrValue "class" (== "info") )
	       `guards`
	       ( getChildren
		 >>>
		 ( ( getChildren >>> hasName "th" >>> xshow getChildren >>> isA hasPart ))
		 `guards`
		 ( getChildren >>> hasName "td" )
	       )
	     )
      ]

-- ------------------------------------------------------------
