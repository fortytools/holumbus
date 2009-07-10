{-# OPTIONS #-}

-- ------------------------------------------------------------

module SimpleIndexer
where
import		 Control.Parallel.Strategies

import           Data.Binary			( Binary )
import qualified Data.Binary			as B			-- else naming conflict with put and get from Monad.State
import		 Data.Char

import		 Holumbus.Crawler.Constants	( )
import		 Holumbus.Index.Documents
import		 Holumbus.Crawler.IndexerCore
import		 Holumbus.Crawler.Html
import		 Holumbus.Crawler.URIs
import		 Holumbus.Crawler.Util

import		 Holumbus.Index.Inverted.Memory

import		 System.Environment

import		 Text.XML.HXT.Arrow
import		 Text.XML.HXT.DOM.Unicode

-- ------------------------------------------------------------

newtype PlainText		= PT { unPT :: String }

instance Binary PlainText where
    get				= B.get >>= (return . PT)
    put				= B.put . unPT

instance NFData PlainText where
    rnf				= rnf . unPT

instance XmlPickler PlainText where
    xpickle 			= xpElem "text" $
				  xpWrap (PT , unPT)
				  xpText0

type SimpleIndexerState		= IndexerState       Inverted Documents PlainText
type SimpleIndexerConfig	= IndexCrawlerConfig Inverted Documents PlainText

simpleIndexerConfig		:: (URI -> Bool)
				-> [IndexContextConfig]
				-> SimpleIndexerConfig
simpleIndexerConfig followRef ixc
				= indexCrawlerConfig
				  [ ]							-- use default read options
				  followRef						-- the set of URIs to be followed and processed 
				  Nothing						-- use default collection filter
				  Nothing						-- use the pre hrefs filter as  pre document filter
				  (Just getHtmlTitle)					-- the document title
				  (Just $ getPlainText128)				-- the customized doc info: the first 128 chars of the the plain text
				  ixc							-- the context configs

simpleIndexer 			:: (URI -> Bool)					-- uris to be processed
                                -> [IndexContextConfig]
				-> [URI]						-- start uris
                                -> IO SimpleIndexerState				-- the index and document table with start of plain text
simpleIndexer refs ixc startUris
				= stdIndexer
				  Nothing
				  startUris
				  ( simpleIndexerConfig refs ixc )
				  ( emptyIndexerState emptyInverted emptyDocuments )

-- ------------------------------------------------------------

siIndexer 			:: IO SimpleIndexerState
siIndexer			= simpleIndexer refs ixc startUris
    where
{-
    startUris			= [ "http://localhost/~si/" ]
    refs			= simpleFollowRef'
				  [ "http://localhost/~si/termine/.*"			-- just 2 subdirs
				  , "http://localhost/~si/Klausuren/.*"
				  , "http://localhost/~si/termine/.*"
				  ]
				  [ ".*[?].*"						-- no query string
				  , "http://localhost/~si/vorlesungen/.*"		-- no lecture pages, currently redundant
				  ]
-}
    startUris			= [ "http://www.fh-wedel.de/~si/vorlesungen/fp/fp.html" ]
    refs			= simpleFollowRef'
				  [ "http://www.fh-wedel.de/~si/vorlesungen/fp/.*.html"
				  ]
				  [ "http://www.fh-wedel.de/~si/vorlesungen/fp/welcome.html"
				  , "http://www.fh-wedel.de/~si/vorlesungen/fp/handouts/.*"
				  , "http://www.fh-wedel.de/~si/vorlesungen/fp/.*[?]VAR=0"
				  , "http://www.fh-wedel.de/~si/vorlesungen/fp/.*/exec.html[?].*"
				  , "http://www.fh-wedel.de/~si/vorlesungen/fp/.*/download[a-zA-Z0-9]*.html[?].*SRC=.*"
				  ]

    ixDefault			=  IndexContextConfig
				   { ixc_name		= "default"
				   , ixc_collectText	= getHtmlPlainText
				   , ixc_textToWords	= deleteNotAllowedChars >>> words
				   , ixc_boringWord	= boringWord
				   }
    ixc				= [ ixDefault
				    { ixc_name		= "title"
				    , ixc_collectText	= getHtmlTitle
				    }
				  , ixDefault
				    { ixc_name		= "headlines"
				    , ixc_collectText	= getAllText getHeadlines
				    }
				  , ixDefault
				    { ixc_name		= "content"
				    , ixc_collectText	= getAllText getBody
				    }
				  ]

getDivCol2, getLecture, getBody, getHeadlines
			:: ArrowXml a => a XmlTree XmlTree

getBody			= this
			  />  hasName "html"
			  />  hasName "body"

getHeadlines		= getBody
			   //> hasNameWith (localPart >>> (`elem` ["h1","h2","h3","h4","h5","h6"]))

getDivCol2		= getBody					-- contents part of fh layout
			  //>
			  ( hasName "div"
			    >>>
			    hasAttrValue "id" (== "col2_content")
			  )

getLecture		= getBody					-- content part of lecture page
			  //>
			  hasAttrValue "id" (== "lecture")

getContText		:: ArrowXml a => a XmlTree XmlTree
getContText		= getDivCol2
			  `orElse`
			  getLecture

getPlainText128		:: ArrowXml a => a XmlTree PlainText
getPlainText128		= getAllText getBody
			  >>^
			  (normalizeWS >>> limitLength 128 >>> PT)

getMetaText 		:: ArrowXml a => a XmlTree String
getMetaText		= getAllText $
			  getXPathTrees "/html/head/meta[@name='description' or @name='keywords']/@content"

-- ------------------------------------------------------------

boringWord		:: String -> Bool
boringWord w		= length w <= 1
			  ||
			  all isXmlDigit w

isAllowedWordChar	:: Char -> Bool
isAllowedWordChar c	= isXmlLetter c
			  ||
			  isXmlDigit c
			  ||
			  c `elem` "_-"

deleteNotAllowedChars	:: String -> String
deleteNotAllowedChars	= map notAllowedToSpace
    where
    notAllowedToSpace c
	| isAllowedWordChar c	= c
	| otherwise		= ' '

-- ------------------------------------------------------------

getOptions			:: [String] -> (Maybe String, String, String)
getOptions ("-r":fn:as)		= (Just fn, s, r)
                                  where
				  (_, s, r) = getOptions as
getOptions (uri : out : _)	= (Nothing, uri, out)
getOptions (uri : _)		= (Nothing, uri, "")
getOptions []			= (Nothing, "", "")

main				:: IO ()
main				= do
				  (_resume, _sid, out) <- getArgs >>= return . getOptions
				  runX $ ( arrIO0 siIndexer
					   >>>
					   xpickleDocument xpickle [(a_indent, v_1)] out
					 )
				  return ()

-- ------------------------------------------------------------
