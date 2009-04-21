module Main(main)
where

import           Control.Applicative
import           Control.Monad		hiding		( when )

import           Holumbus.Build.Config
import           Holumbus.Build.Crawl
import           Holumbus.Build.Index
import           Holumbus.Index.Common
import           Holumbus.Index.Documents
import           Holumbus.Index.Inverted.Memory(emptyInverted)
import           Holumbus.Index.Cache
import           Holumbus.Utility

import           Data.Char
import           Data.Maybe
import           Data.List

import           System.IO

import           Text.XML.HXT.Arrow	hiding 		( getXPathTrees )
import           Text.XML.HXT.Arrow.XPathSimple 	()	-- ( getXPathTrees )
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
import           Text.XML.HXT.DOM.Unicode

-- ------------------------------------------------------------

main :: IO ()
main
  = do
    let traceLevel     = 1
        workerThreads  = 1
        docsPerCrawl   = 10
        -- docsPerIndex   = (250::Int)
        idxConfig      = ic_si -- ic_fhw
        crawlerState   = initialCrawlerState idxConfig emptyDocuments customFunction

    -- ---------------------------------------------------------------------------------------------
    -- CRAWLING
    -- ---------------------------------------------------------------------------------------------
    trcMsg " crawling  ----------------------------- "
    docs       <- crawl traceLevel workerThreads docsPerCrawl crawlerState
    localDocs <- return $ tmpDocs (fromMaybe "/tmp" (ic_tempPath idxConfig)) docs

    writeToBinFile ( (ic_indexPath idxConfig) ++ "-docs.bin") (docs)

    -- ---------------------------------------------------------------------------------------------
    -- INDEXING
    -- ---------------------------------------------------------------------------------------------
    trcMsg " indexing  ----------------------------- "

    c <- createCache ((ic_indexPath idxConfig) ++ "-cache.db")

    idx <- buildIndex workerThreads traceLevel localDocs idxConfig emptyInverted (Just c)

    writeToXmlFile ( (ic_indexPath idxConfig) ++ "-index.xml") idx
    writeToBinFile ( (ic_indexPath idxConfig) ++ "-index.bin") idx

    return()

customFunction :: ArrowXml a => a XmlTree (Maybe Int)
customFunction = constA Nothing

-- ------------------------------------------------------------
--
-- useful helpers

wordList 	:: String -> [String]
wordList	= map (stripWith (=='.')) . (parseWords isWordChar)

tooShort 	:: [a] -> Bool
tooShort	= (< 2) . length

noLetter 	:: String -> Bool
noLetter	= not . any isXmlLetter

simpleCrawlFilter'	:: (String -> Bool) -> (String -> Bool) -> (String -> Bool)
simpleCrawlFilter' isAllowed isDenied
    		= isAllowed .&&. (not . isDenied)
		  where
		  (.&&.) = liftA2 (&&)

mkAlt		:: [String] -> String
mkAlt rs	= "(" ++ intercalate "|" rs ++ ")"

-- ------------------------------------------------------------

getTitle, getBody, getDescrOrKeywords
		:: ArrowXml a => a XmlTree XmlTree

getTitle	= getByPath ["html", "head", "title"]

getBody		= getByPath ["html", "body"]

getDescrOrKeywords
		= getByPath ["html", "head", "meta"]
		  >>>
		  hasAttrValue "name" (`elem` ["description", "keywords"])
		  >>>
		  getAttrl >>> hasName "content"

-- ------------------------------------------------------------

getDivCol2, getLecture
		:: ArrowXml a => a XmlTree XmlTree
getDivCol2	= getBody					-- fh layout content part
		  >>>
		  deep ( hasName "div"
			 >>>
			 hasAttrValue "id" (== "col2_content")
		       )

getLecture	= getBody					-- lecture content part
		  >>>
		  hasAttrValue "id" (== "lecture")

-- ------------------------------------------------------------

homeSi		:: String
-- homeSi		= "http://www.fh-wedel.de/~si/"
homeSi		= "http://localhost/~si/"

isAllowedSi	:: String -> Bool
isAllowedSi
    = match $ homeSi ++ mkAlt ok ++ "/.*[.]html"
    where
    ok = [ "termine"
	 , "vorlesungen/fp"
	 , "vorlesungen/softwaredesign"
	 , "vorlesungen/java"
	 , "praktika/SoftwarePraktikum"
	 , "klausuren"
	 ]

isDeniedSi	:: String -> Bool
isDeniedSi
    = match $ homeSi ++ mkAlt nok ++ ".*"
    where
    nok = [ "praktika/SoftwarePraktikum/Loesungen/"
	  , "praktika/SoftwarePraktikum/photoalbum2?/"
	  , "vorlesungen/[^/]*/welcome.html"
	  ]

-- ------------------------------------------------------------

ic_si :: IndexerConfig
ic_si
    = IndexerConfig
      { ic_startPages	  = [ homeSi
			    -- , homeSi ++ "vorlesungen/fp/Einleitung/index.html"
			    ]
      , ic_indexerTimeOut = 120 * 1000000		-- 2 minutes
      , ic_readAttributes = standardReadDocumentAttributes
      , ic_contextConfigs = ccs_si
      , ic_fCrawlFilter	  = simpleCrawlFilter' isAllowedSi isDeniedSi
      , ic_indexPath      = "./index/si"
      , ic_tempPath       = Just "./tmp/si/"
      }

ccs_si	:: [ContextConfig]
ccs_si
    = [ si_title
      , si_meta
      , si_content
      ]

si_default, si_title, si_meta, si_content :: ContextConfig

si_default
    = ContextConfig
      { cc_name         = "raw"
      , cc_preFilter    = this
      , cc_fExtract     = hasName "body"
      , cc_fTokenize    = wordList
      , cc_fIsStopWord  = tooShort .||. noLetter
      , cc_addToCache   = False
      }
    where
    (.||.) = liftA2 (||)

si_title
    = si_default
      { cc_name		= "title"
      , cc_fExtract	= getTitle
      }

si_meta
    = si_default
      { cc_name		= "meta"
      -- , cc_fExtract     = getXPathTrees "/html/head/meta[@name='description' or @name='keywords']/@content"
      , cc_fExtract	= getDescrOrKeywords
      }

si_content
    = si_default
      { cc_name		= "content"
      , cc_fExtract	= getDivCol2		-- FH layout pages
	                  `orElse`
	                  getLecture		-- or pages of lecture document
      }

-- ------------------------------------------------------------
