{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Holumbus.Crawler.Core
import           Holumbus.Crawler.URIChecker

import		 Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import           System.IO
import		 System.Environment
import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

check_localhost_si 	:: (String, URIClassList)
check_localhost_si	= ( "http://localhost/~si/"
			  , [ ("http://localhost/~si/.*/welcome[.]html", 		Manual)
			    , ("http://localhost/~si/index*[.]html", 			Contents)
			    , ("http://localhost/~si.*[.]html[?].*",      		Manual)
			    , ("http://localhost/~si.*[.]html",				Exists)
			    , ("http://localhost/~si/.*[.](gif|jpg|css|ico|pdf)",	Exists)
			    , ("http://localhost/.*", 					Manual)
			    , ("http://.*", 						Manual)
			    , ("mailto:.*", 						Manual)
			    , ("https://.*", 						Manual)
			    , ("javascript:.*", 					Illegal)
			    , ("file:///.*", 						Illegal)
			    , ( ".*",							Illegal)
			    ]
			  )

-- ------------------------------------------------------------

getOptions		:: [String] -> (Maybe String, String)
getOptions ("-r":fn:as)	= (Just fn, r)
			  where
			  (_, r) = getOptions as
getOptions (out:_)	= (Nothing, out)
getOptions []		= (Nothing, "")

main	:: IO ()
main	= do
	  (resume, _out) <- getArgs >>= return . getOptions
	  dm 		<- uncurry (simpleURIChecker resume) check_localhost_si
	  putStrLn      $ show dm
{-
	  runX ( constA (getS theResultAccu $ docs)
		 >>>
		 xpickleVal (xpTextDocs "http://localhost/~si/")
		 >>>
		 addXHtmlDoctypeTransitional
		 >>>
		 writeDocument [ (a_indent, v_1)
			       , (a_output_encoding, usAscii)
			       , (a_output_html, v_1)
			       ] out
	       )
-}
	  return ()

resultTemplate		:: IOSArrow a XmlTree
resultTemplate		= readDocument [ (a_parse_xml, v_1)
				       , (a_validate, v_0)
				       , (a_remove_whitespace, v_1)
				       ] "resultTemplate.html"

genResultPage		:: String -> URI -> URIClassList -> DocMap -> IOSArrow a XmlTree
genResultPage out uri ucs dm
			= resultTemplate
			  >>>
			  fromLA (genPage handlers1)
			  >>>
			  addXHtmlDoctypeTransitional
			  >>>
			  writeDocument [ (a_indent, v_1)
					, (a_output_encoding, usAscii)
					, (a_output_html, v_1)
					] out
    where
    dmIx		= zip (M.keys $ dm) [(1::Int) ..]
    errURIs		= map fst . filter (containsErrURIs . snd) . M.toList $ dm
			  where
			  containsErrURIs d
			      = any isNotOk . S.toList . dd_uris $ d
				where
				isNotOk u2
				    = (/= "200") . dd_status . (M.! u2) $ dm

    classURIs cf	= map fst . filter (cf . dd_class . snd) . M.toList $ dm

    genPage handlers	= processTopDownUntil (hasHandler `guards` applyHandler)
			  where
			  hasHandler	= getAttrValue0 "id" >>> isA (`elem` (map fst handlers))
			  applyHandler	= af $< getAttrValue "id"
			                  where
					  af k	= removeAttr "id"
						  >>>
						  (fromMaybe this . lookup k $ handlers)

    handlers1		= [ ("title", 		txt (show uri))
			  , ("h1-title",	insertPageURI uri)
			  , ("error-index",	insertErrorIndex)
			  , ("error-uri-link",  insertErrorURIix)
			  , ("content-part",    insertPart (== Contents))
			  , ("exists-part",     insertPart (== Exists))
			  , ("notok-part",      insertPart (== Not200OK))
			  , ("manual-part",     insertPart (== Manual))
			  , ("illegal-part",    insertPart (== Illegal))
			  ]
    handlers2a          = [ ("error-uri-link",  insertErrorURIix) ]
    handlers2b uf       = [ ("page-descr",      insertPageDescr uf) ]
    handlers3b uri'	= [ ("page-uri",        insertPageURI  uri')
			  , ("page-data",       insertPageData uri')
			  , ("page-uris",       insertPageURIs uri')
			  ]

    insertErrorIndex
	| noErrors	= txt ""
	| otherwise	= getChildren >>> genPage handlers2a
	where
	noErrors	= null errURIs

    insertErrorURIix    = uf $< constL errURIs
			  where
			  uf uri' = replaceChildren $
				    eelem "a" += sattr "href" (("#uri-" ++) . show . fromJust . lookup uri' $ dmIx)
				              += txt (show uri')

    insertPart	cf      = getChildren >>> genPage (handlers2b cf)

    insertPageDescr cf  = uf $< constL (classURIs cf)
			  where
			  uf uri' = getChildren >>> genPage (handlers3b uri')

    insertPageURI uri'	= replaceChildren $
			  eelem "a" += sattr "href" uri'
				    += txt (show uri')

    insertPageData uri' = this
    insertPageURIs uri' = this

-- ------------------------------------------------------------
