{-# OPTIONS #-}

-- ------------------------------------------------------------

-- module SimpleIndexer
module Main
where
import           Control.DeepSeq

import           Data.Binary                    ( Binary )
import qualified Data.Binary                    as B                    -- else naming conflict with put and get from Monad.State

import		 Holumbus.Crawler
import		 Holumbus.Crawler.IndexerCore
import		 Holumbus.Crawler.PdfToText

import           Holumbus.Index.Documents       ( Documents(..)
                                                , emptyDocuments
                                                )
import		 Holumbus.Index.Inverted.PrefixMem

import           System.Environment

import		 Text.XML.HXT.Arrow		hiding ( readDocument )
import           Text.XML.HXT.XPath
import           Text.XML.HXT.Arrow.XmlCache
import		 Text.XML.HXT.DOM.Unicode

-- ------------------------------------------------------------

newtype PlainText               = PT { unPT :: String }

instance Binary PlainText where
    get                         = B.get >>= (return . PT)
    put                         = B.put . unPT

instance NFData PlainText where
    rnf                         = rnf . unPT

instance XmlPickler PlainText where
    xpickle                     = xpElem "text" $
                                  xpWrap (PT , unPT)
                                  xpText0

-- ------------------------------------------------------------

type SimpleIndexerState         = IndexerState       Inverted Documents PlainText
type SimpleIndexerConfig        = IndexCrawlerConfig Inverted Documents PlainText

simpleIndexerConfig             :: (URI -> Bool)
                                -> [IndexContextConfig]
                                -> SimpleIndexerConfig
simpleIndexerConfig followRef ixc
				= indexCrawlerConfig
				  [ (a_cache, 	"./cache"	)			-- local cache dir "cache"
				  , (a_compress, v_1		)			-- cache files will be compressed
				  , (a_document_age,
					 show $ (60 * 60 * 24 * 30::Integer))		-- cache remains valid 30 months
                                  , (a_accept_mimetypes, 	unwords [text_html, application_xhtml, application_pdf])
				  , (a_parse_html,              v_0)
				  , (a_parse_by_mimetype,	v_1)
				  ]                                                     -- use default read options, but accept pdfs too
				  followRef						-- the set of URIs to be followed and processed 
				  Nothing						-- use default collection filter
                                  (Just $ checkDocumentStatus >>> preDocumentFilter)    -- filter for pdf extraction
                                  (Just getTitleOrDocName)                              -- the document title
				  (Just $ getPlainText128)				-- the customized doc info: the first 128 chars of the the plain text
				  ixc							-- the context configs

simpleIndexer                   :: (URI -> Bool)                                        -- uris to be processed
                                -> [IndexContextConfig]
                                -> [URI]                                                -- start uris
                                -> IO SimpleIndexerState                                -- the index and document table with start of plain text
simpleIndexer refs ixc startUris
                                = stdIndexer
                                  Nothing
                                  startUris
                                  ( setCrawlerTraceLevel indexerTraceLevel $
                                    setCrawlerSaveConf indexerSaveIntervall indexerSavePath $
                                    setCrawlerMaxDocs indexerMaxDocs indexerMaxParDocs $
                                    simpleIndexerConfig refs ixc
                                  )
                                  ( emptyIndexerState emptyInverted emptyDocuments )

indexerSaveIntervall		:: Int
indexerSaveIntervall		= 1000

indexerSavePath			:: String
indexerSavePath			= "./tmp/ix-"

indexerTraceLevel		:: Priority
indexerTraceLevel		= NOTICE

indexerMaxDocs			:: Int
indexerMaxDocs			= 5000

indexerMaxParDocs		:: Int
indexerMaxParDocs		= 10

-- ------------------------------------------------------------

siIndexer                       :: IO SimpleIndexerState
siIndexer                       = simpleIndexer refs ixc startUris
    where
{-
    startUris                   = [ "http://localhost/~si/" ]
    refs                        = simpleFollowRef'
                                  [ "http://localhost/~si/termine/.*"                   -- just 2 subdirs
                                  , "http://localhost/~si/Klausuren/.*"
                                  , "http://localhost/~si/termine/.*"
                                  ]
                                  [ ".*[?].*"                                           -- no query string
                                  , "http://localhost/~si/vorlesungen/.*"               -- no lecture pages, currently redundant
                                  ]
    startUris                   = [ "tmp.pdf"
				  -- , "http://www.fh-wedel.de/~si/vorlesungen/fp/fp.html"
				  -- , "http://www.fh-wedel.de/~si/vorlesungen/fp/handouts/vortraege/ws2004/AbstrakteDatentypen.pdf"
				  ]
-}
{- vorlesungen
    startUris                   = [ "http://www.fh-wedel.de/~si/vorlesungen/fp/index.html"
				  , "http://www.fh-wedel.de/~si/vorlesungen/java/index.html"
				  , "http://www.fh-wedel.de/~si/vorlesungen/cb/index.html"
				  , "http://www.fh-wedel.de/~si/vorlesungen/c/index.html"
				  , "http://www.fh-wedel.de/~si/vorlesungen/internet/index.html"
				  , "http://www.fh-wedel.de/~si/vorlesungen/softwaredesign/index.html"
				  ]
    refs                        = simpleFollowRef'
                                  [ vl ++ ".*[.](html|pdf)"
                                  ]
                                  ( map (vl ++) ["welcome[.]html"
						, "handouts/.*.html"
						, ".*[?]VAR=0"
						, "(.*/)?exec[.]html[?].*"
						, ".*/download[a-zA-Z0-9]*[.]html[?].*SRC=.*"
						]
				  )
                                  where
				  vl = "http://www[.]fh-wedel[.]de/~si/vorlesungen/(c|cb|fp|internet|java|softwaredesign)/"
-}
    startUris                   = [ "http://www.fh-wedel.de/~si/index.html"
				  ]
    refs                        = simpleFollowRef'
                                  [ v1 ++ ".*[.](html|pdf)" ]
                                  ( map (v1 ++) ["welcome[.]html"
						, "handouts/.*.html"
						, ".*[?]VAR=0"
						, "(.*/)?exec[.]html[?].*"
						, ".*/download[a-zA-Z0-9]*[.]html[?].*SRC=.*"
						]
				  )
                                  where
				  v1 = "http://www[.]fh-wedel[.]de/~si/(seminare|klausuren|praktika|projekte|termine|zettelkasten|vorlesungen/(c|cb|fp|internet|java|softwaredesign))/"
				  -- v1 = "http://www[.]fh-wedel[.]de/~si/(-seminare|klausuren|-praktika|-projekte|termine|zettelkasten|-vorlesungen/(c|cb|fp|internet|java|softwaredesign))/"

    ixDefault                   =  IndexContextConfig
                                   { ixc_name           = "default"
                                   , ixc_collectText    = getHtmlPlainText
                                   , ixc_textToWords    = deleteNotAllowedChars >>> words
                                   , ixc_boringWord     = boringWord
                                   }
    ixc                         = [ ixDefault
                                    { ixc_name          = "title"
                                    , ixc_collectText   = getHtmlTitle
                                    }
                                  , ixDefault
                                    { ixc_name          = "headlines"
                                    , ixc_collectText   = getAllText getHeadlines
                                    }
                                  , ixDefault
                                    { ixc_name          = "content"
                                    , ixc_collectText   = getAllText getBody
                                    }
				  , ixDefault
                                    { ixc_name           = "pdf"
                                    , ixc_collectText    = getAllText (isPdfContents `guards` getChildren)
                                    , ixc_textToWords    = deleteNotAllowedChars >>> words
                                    , ixc_boringWord     = boringWord
				    }
                                  ]

-- ------------------------------------------------------------

preDocumentFilter	:: IOSArrow XmlTree XmlTree
preDocumentFilter	= choiceA
			  [ isHtmlContents	:-> this			-- do nothing
			  , isPdfContents	:-> extractPdfText		-- extract the text from a pdf
			  , this		:-> replaceChildren none	-- throw away every contents
			  ]
    where
    extractPdfText	= traceDoc "extractPdfText: start"
			  >>>
			  processChildren ( deep getText >>> pdfToTextA >>> mkText )
			  >>>
			  traceDoc "extractPdfText: result"

getDivCol2
  , getLecture
  , getBody
  , getHeadlines        :: ArrowXml a => a XmlTree XmlTree

getBody                 = this
                          />  hasName "html"
                          />  hasName "body"

getHeadlines            = getBody
                           //> hasNameWith (localPart >>> (`elem` ["h1","h2","h3","h4","h5","h6"]))

getDivCol2              = getBody                                       -- contents part of fh layout
                          //>
                          ( hasName "div"
                            >>>
                            hasAttrValue "id" (== "col2_content")
                          )

getLecture              = getBody                                       -- content part of lecture page
                          //>
                          hasAttrValue "id" (== "lecture")

getContText             :: ArrowXml a => a XmlTree XmlTree
getContText             = getDivCol2
                          `orElse`
                          getLecture

getPlainText128         :: ArrowXml a => a XmlTree PlainText
getPlainText128         = getAllText getBody
                          >>^
                          (normalizeWS >>> limitLength 128 >>> PT)

getMetaText             :: ArrowXml a => a XmlTree String
getMetaText             = getAllText $
                          getXPathTrees "/html/head/meta[@name='description' or @name='keywords']/@content"

-- ------------------------------------------------------------

boringWord              	:: String -> Bool
boringWord w            	= length w <= 1
				  ||
				  not (any isXmlLetter w)

isAllowedWordChar       	:: Char -> Bool
isAllowedWordChar c     	= isXmlLetter c
				  ||
				  isXmlDigit c
				  ||
				  c `elem` "_-"

deleteNotAllowedChars  		:: String -> String
deleteNotAllowedChars   	= map notAllowedToSpace
    where
    notAllowedToSpace c
        | isAllowedWordChar c   = c
        | otherwise             = ' '

-- ------------------------------------------------------------

getOptions                      :: [String] -> (Maybe String, String, String)
getOptions ("-r":fn:as)         = (Just fn, s, r)
                                  where
                                  (_, s, r) = getOptions as
getOptions (uri : out : _)      = (Nothing, uri, out)
getOptions (uri : _)            = (Nothing, uri, "")
getOptions []                   = (Nothing, "", "")

main                            :: IO ()
main                            = do
                                  (_resume, _sid, out) <- getArgs >>= return . getOptions
                                  runX ( hxtSetTraceAndErrorLogger NOTICE
                                         >>>
                                         arrIO0 siIndexer
                                         >>>
                                         traceMsg 0 (unwords ["writing index into XML file", out])
                                         >>>
                                         xpickleDocument xpickle [(a_indent, v_1)] out
                                         >>>
                                         traceMsg 0 "writing index finished"

                                       )
                                    >> return ()

-- ------------------------------------------------------------
