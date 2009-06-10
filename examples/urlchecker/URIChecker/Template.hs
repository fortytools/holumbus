{-# OPTIONS #-}

-- ------------------------------------------------------------

module URIChecker.Template
where

import           Holumbus.Crawler.Core
import           Holumbus.Crawler.URIChecker

import		 Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

-- | A simple template scheme is used for generating the HTML result page

genResultPage		:: String -> URI -> URIClassList -> DocMap -> IOSArrow a XmlTree
genResultPage out uri _ucs dm
			=  readDocument [ (a_parse_xml, v_1)
				       , (a_validate, v_0)
				       , (a_remove_whitespace, v_1)
				       ] "resultTemplate.html"			-- read the template
			  >>>
			  fromLA (genPage handlers1)				-- traverse the template and fill in the holes by the handlers1 list
			  >>>
			  addXHtmlDoctypeTransitional
			  >>>
			  writeDocument [ (a_indent, v_1)
					, (a_output_encoding, usAscii)
					, (a_output_html, v_1)
					, (a_no_empty_elements, v_1)
					] out
    where
    dmIx		= zip (M.keys $ dm) [(1::Int) ..]
    lookupDmIx uri'     = ("uri-" ++) . show . fromJust . lookup uri' $ dmIx
    errURIs		= map fst . filter (containsErrURIs . snd) . M.toList $ dm
			  where
			  containsErrURIs d
			      = any isNotOk . S.toList . dd_uris $ d
				where
				isNotOk u2
				    = (/= "200") . dd_status . (M.! u2) $ dm

    classURIs cf	= map fst . filter (cf . dd_class . snd) . M.toList $ dm

    genPage handlers	= processTopDownUntil (hasHandler `guards` applyHandler)		-- the template mechanism
			  where									-- elements with id attributes are searched
			  hasHandler	= getAttrValue0 "id" >>> isA (`elem` (map fst handlers))-- if the id value is key of the handler list, the handler is applied to the tree
			  applyHandler	= af $< getAttrValue "id"				-- but before that the id attribute is removed
			                  where
					  af k	= removeAttr "id"
						  >>>
						  (fromMaybe this . lookup k $ handlers)

    handlers1		= [ ("title", 		txt uri)
			  , ("h1-title",	insertPageURI0 uri >>> getChildren)
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
			  , ("page-status",     insertPageStatus uri')
			  , ("page-mimetype",   insertMimeType uri')
			  ]
    handlers4b uris'    = [ ("page-uri1",       insertURIsRefs uris') ]

    insertErrorIndex
	| noErrors	= txt ""
	| otherwise	= getChildren >>> genPage handlers2a
	where
	noErrors	= null errURIs

    insertErrorURIix    = insertPageURIx $< constL errURIs

    insertPart	cf      = if null clsURIs
			  then txt ""
			  else getChildren >>> genPage (handlers2b cf)
			  where
			  clsURIs = classURIs cf

    insertPageDescr cf  = if null clsURIs
			  then txt ""
			  else uf $< constL clsURIs
			  where
			  uf uri' = getChildren >>> genPage (handlers3b uri')
			  clsURIs = classURIs cf

    insertPageURI uri'	= insertPageURI0 uri'
			  >>>
			  ( replaceChildren $
			    ( eelem "a" += sattr "name" (lookupDmIx uri') )
			    <+>
			    getChildren
			  )

    insertPageURIx uri'	= replaceChildren $
			  ( eelem "a" += sattr "href" (("#" ++) . lookupDmIx $ uri')
				      += txt uri'
			  )

    insertPageURI0 uri'	= replaceChildren $
			  ( eelem "a" += sattr "href" uri'
				      += txt uri'
			  )

    insertPageStatus uri'= replaceChildren $
			  txt $ unwords [dd_status dd, dd_message dd]
			  where
			  dd = fromJust . M.lookup uri' $ dm

    insertMimeType uri'	= replaceChildren $
			  txt $ dd_mimetype dd
			  where
			  dd = fromJust . M.lookup uri' $ dm

    insertPageData uri' = replaceChildren $
			  txt $ concat $
				  ["status: ", st, " ", ms]
				  ++
			          (if null mt then [] else [", ", mt])
				  ++
				  (if null md then [] else [", ", md])
			  where
			  dd = fromJust . M.lookup uri' $ dm
			  st = dd_status   dd
			  ms = dd_message  dd
			  mt = dd_mimetype dd
			  md = dd_modified dd

    insertPageURIs uri' = if null uris'
			  then none
			  else processChildren (genPage $ handlers4b uris')
			  where
			  uris' = S.toList . dd_uris . fromJust . M.lookup uri' $ dm

    insertURIsRefs uris'= uf $< constL uris'
		          where
			  uf uri' = (insertPageURI0 uri')
				    += st
				    where
				    dd' = fromJust . M.lookup uri' $ dm
				    ds' = dd_status  dd'
				    dc' = dd_class   dd'
				    dm' = dd_message dd'
				    ms' | dc' `elem` [Manual, Illegal] 	= dm'
				        | null dm'		       	= show dc'
					| otherwise			= ds' ++ " " ++ dm'

				    st  | ds' == "200"
					  ||
					  dc' == Ignore
					                = none
					| otherwise	= eelem "span" += sattr "class" "error"
					                               += txt ("(" ++ ms' ++ ")")

-- ------------------------------------------------------------
