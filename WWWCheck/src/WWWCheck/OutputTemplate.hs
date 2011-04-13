{-# OPTIONS #-}

-- ------------------------------------------------------------

module WWWCheck.OutputTemplate
where

import           Data.Maybe
import qualified Data.Map as M

import           Holumbus.Crawler.URIs

import           System.FilePath        ( (</>) )

import           Text.XML.HXT.Core

import           WWWCheck.URIChecker

-- ------------------------------------------------------------

-- | A simple template scheme is used for generating the HTML result page

type ReverseRefs        = M.Map URI URIs

revRefs                 :: DocMap -> ReverseRefs
revRefs                 = M.foldWithKey insRefs M.empty
    where
    insRefs uri dd rm   = foldURIs insRef rm $ dd_uris dd
        where
        insRef uri' rm' = M.insertWith unionURIs uri' (singletonURIs uri) rm'

lookupURI               :: URI -> DocMap -> DocDescr
lookupURI uri           = fromMaybe (invalidDocDescr {dd_message = "rejected by robots.txt"}) . M.lookup uri

genResultPage           :: String -> String -> URI -> URIClassList -> IOSArrow DocMap XmlTree
genResultPage templateBase out uri ucs
                        = genResultPage' templateBase out uri ucs $< this

genResultPage'          :: String -> String -> URI -> URIClassList -> DocMap -> IOSArrow a XmlTree
genResultPage' templateBase out uri _ucs dm
                        =  readDocument [ withParseHTML no
                                        , withValidate no
                                        , withRemoveWS yes
                                        ] (templateBase </> "resultTemplate.html") -- read the template
                          >>>
                          fromLA (genPage handlers1)                            -- traverse the template and fill in the holes by the handlers1 list
                          >>>
                          addXHtmlDoctypeTransitional
                          >>>
                          writeDocument [ withIndent yes
                                        , withOutputEncoding usAscii
                                        , withOutputXHTML
                                        ] out
    where
    dmIx                = zip (M.keys $ dm) [(1::Int) ..]
    lookupDmIx uri'     = ("uri-" ++) . show . fromJust . lookup uri' $ dmIx
    revRefMap           = revRefs dm

    errURIs             = map fst . filter (containsErrURIs . snd) . M.toList $ dm
                          where
                          containsErrURIs d
                              = any isNotOk . toListURIs . dd_uris $ d
                                where
                                isNotOk u2
                                    = ( u2 `M.notMember` dm )
                                      ||
                                      ( dc' `elem` [Not200OK, Illegal] )
                                      ||
                                      ( dc' `elem` [Contents, Exists] && ds' /= "200")
                                      where
                                      dd' = dm M.! u2
                                      dc' = dd_class  dd'
                                      ds' = dd_status dd'

    classURIs cf        = map fst . filter (cf . dd_class . snd) . M.toList $ dm

    genPage handlers    = processTopDownUntil (hasHandler `guards` applyHandler)                -- the template mechanism
                          where                                                                 -- elements with id attributes are searched
                          hasHandler    = getAttrValue0 "id" >>> isA (`elem` (map fst handlers))-- if the id value is key of the handler list, the handler is applied to the tree
                          applyHandler  = af $< getAttrValue "id"                               -- but before that the id attribute is removed
                                          where
                                          af k  = removeAttr "id"
                                                  >>>
                                                  (fromMaybe this . lookup k $ handlers)

    handlers1           = [ ("title",           txt uri)
                          , ("h1-title",        insertPageURI0 uri >>> getChildren)
                          , ("error-index",     insertErrorIndex)
                          , ("error-uri-link",  insertErrorURIix)
                          , ("content-part",    insertPart (== Contents))
                          , ("exists-part",     insertPart (== Exists))
                          , ("notok-part",      insertPart (== Not200OK))
                          , ("manual-part",     insertPart (== Manual))
                          , ("illegal-part",    insertPart (== Illegal))
                          ]
    handlers2a          = [ ("error-uri-link",  insertErrorURIix)
                          , ("error-uri-cnt",   insertURIcnt $ length errURIs)
                          ]
    handlers2b uf       = [ ("page-descr",      insertPageDescr uf)
                          , ("page-cnt",        insertPageCnt   uf)
                          ]
    handlers3b uri'     = [ ("page-uri",        insertPageURI  uri')
                          , ("page-data",       insertPageData uri')
                          , ("page-uris",       insertPageURIs uri')
                          , ("page-status",     insertPageStatus uri')
                          , ("page-mimetype",   insertMimeType uri')
                          , ("uri-refs",        insertURIRefs uri')
                          ]
    handlers4b uris'    = [ ("page-uri1",       insertURIsRefs uris')
                          , ("page-uri0",       insertURIsRefs0 uris')
                          ]

    insertErrorIndex
        | noErrors      = txt ""
        | otherwise     = getChildren >>> genPage handlers2a
        where
        noErrors        = null errURIs

    insertErrorURIix    = insertPageURIx $< constL errURIs

    insertURIcnt 0      = txt "no URI"
    insertURIcnt 1      = txt "1 URI"
    insertURIcnt n      = txt $ (show n) ++ " URIs"

    insertPart  cf      = if null clsURIs
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

    insertPageCnt cf    = insertURIcnt . length . classURIs $ cf

    insertPageURI uri'  = insertPageURI0 uri'
                          >>>
                          ( replaceChildren $
                            ( eelem "a" += sattr "name" (lookupDmIx uri') )
                            <+>
                            getChildren
                          )

    insertPageURIx uri' = replaceChildren $
                          ( eelem "a" += sattr "href" (("#" ++) . lookupDmIx $ uri')
                                      += txt uri'
                          )

    insertPageURI0 uri' = replaceChildren $
                          ( eelem "a" += sattr "href" uri'
                                      += txt uri'
                          )

    insertPageStatus uri'= replaceChildren $
                          txt $ unwords [dd_status dd, dd_message dd]
                          where
                          dd = lookupURI uri' $ dm

    insertMimeType uri' = replaceChildren $
                          txt $ dd_mimetype dd
                          where
                          dd = lookupURI uri' $ dm

    insertPageData uri' = replaceChildren $
                          txt $ concat $
                                  ["status: ", st, " ", ms]
                                  ++
                                  (if null mt then [] else [", ", mt])
                                  ++
                                  (if null md then [] else [", ", md])
                          where
                          dd = lookupURI uri' $ dm
                          st = dd_status   dd
                          ms = dd_message  dd
                          mt = dd_mimetype dd
                          md = dd_modified dd

    insertPageURIs uri' = if null uris'
                          then txt ""
                          else processChildren (genPage $ handlers4b uris')
                          where
                          uris' = toListURIs . dd_uris . lookupURI uri' $ dm

    insertURIsRefs uris'= uf $< constL uris'
                          where
                          uf uri' = (insertPageURI0 uri')
                                    += st
                                    where
                                    dd' = lookupURI uri' $ dm
                                    ds' = dd_status  dd'
                                    dc' = dd_class   dd'
                                    dm' = dd_message dd'
                                    ms' | dc' `elem` [Manual, Illegal]  = dm'
                                        | null dm'                      = show dc'
                                        | otherwise                     = ds' ++ " " ++ dm'

                                    st  | ds' == "200"
                                          ||
                                          dc' == Ignore
                                                        = none
                                        | otherwise     = eelem "span" += sattr "class" "error"
                                                                       += txt ("(" ++ ms' ++ ")")

    insertURIRefs uri'  = if null uris'
                          then txt ""
                          else processChildren (genPage $ handlers4b uris')
                          where
                          uris' = toListURIs . fromMaybe emptyURIs . M.lookup uri' $ revRefMap

    insertURIsRefs0 u' = uf $< constL u'
                         where
                         uf uri' = (insertPageURIx uri')

-- ------------------------------------------------------------
