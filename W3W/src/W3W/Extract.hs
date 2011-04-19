{-# OPTIONS #-}

-- ------------------------------------------------------------

module W3W.Extract
where

import           Data.Char.Properties.XMLCharProps
import           Data.Char                            ( toLower )
import           Data.List                            ( isPrefixOf )
import           Data.Maybe

import		 Holumbus.Crawler

import		 Text.XML.HXT.Core


-- ------------------------------------------------------------
--
-- the arrows for building the index
--
-- ------------------------------------------------------------

getHeadlines                    :: ArrowXml a => a XmlTree String
getHeadlines                    = fromLA     $
                                  getAllText $
                                  ( getByPath ["html", "body"]
                                    //>
                                    ( (isElem >>> getLocalPart >>> isA isHname)
                                      `guards`
                                      getChildren
                                    )
                                  )
    where
    isHname ('h':d:[])		= d `elem` "123456"
    isHname _                   = False

-- ------------------------------------------------------------

getContentText                  :: IOSArrow XmlTree String
getContentText                  =  choiceA
                                   [ isHtmlContents :-> getHtmlText
                                   , isPdfContents  :-> none	-- not yet implemented
                                   , this           :-> none
                                   ]

getHtmlText                     :: IOSArrow XmlTree String
getHtmlText                     = getAllText $
                                  choiceA
                                  [ isFhwLayout     :-> ( traceMsg 1 "fhw layout found"
                                                          >>>
                                                          deep (hasDivWithId "col3_content")
                                                        )
                                  , isEgLayout      :-> ( traceMsg 1 "MartinEgge's layout found"
                                                          >>>
                                                          deep (hasDivWithId "PageContentDiv")
                                                        )
                                  , isSiLayout      :-> ( traceMsg 1 "Uwe's layout found"
                                                          >>>
                                                          deep (hasDivWithId "col2_content")
                                                        )
                                  , isPtlLayout     :-> ( traceMsg 1 "ptl layout found"
                                                          >>>
                                                          deep (hasDivWithId "col2_content")
                                                        )
                                  , this            :-> ( traceMsg 1 "unknown layout found"
                                                          >>>
                                                          getByPath ["html", "body"]
                                                        )
                                  ]

getURI                          :: ArrowXml a => a XmlTree String
getURI                          = fromLA $ getAttrValue transferURI

getDates                        :: ArrowXml a => a XmlTree String
getDates                        = none		-- not yet implemented

-- ------------------------------------------------------------

-- predicate arrows

isXxxLayout                     :: ArrowXml a => String -> a XmlTree XmlTree
isXxxLayout xxx                 = fromLA $
                                  getLink				-- hack
                                  >>>
                                  hasAttrValue "rel" (== "shortcut icon")
                                  >>>
                                  hasAttrValue "href" (== xxx)

isFhwLayout                     :: ArrowXml a => a XmlTree XmlTree
isFhwLayout                     = isXxxLayout "fileadmin/templates/fhw_images/favicon.ico"

isPtlLayout                     :: ArrowXml a => a XmlTree XmlTree
isPtlLayout                     = isXxxLayout "fileadmin/templates/ptl_images/favptlicon.ico"

isEgLayout                      :: ArrowXml a => a XmlTree XmlTree
isEgLayout                      = fromLA $
                                  ( getMetaAttr "author"
                                    >>>
                                    isA ("Martin Egge" `isPrefixOf`)
                                  )
                                  `guards` this

isSiLayout                      :: ArrowXml a => a XmlTree XmlTree                                  
isSiLayout                      = fromLA $
                                  ( getMetaAttr "keywords"			-- hack: ther should be a meta elem for author 
                                    >>>
                                    isA ("Uwe Schmidt" `isPrefixOf`)
                                  )
                                  `guards` this

-- ------------------------------------------------------------
--
-- mothers little helpers

hasNameWithId			:: ArrowXml a => String -> String -> a XmlTree XmlTree
hasNameWithId ename eid		= isElem
                                  >>>
                                  hasName ename
                                  >>>
                                  hasAttrValue "id" (== eid)

hasDivWithId			:: ArrowXml a => String -> a XmlTree XmlTree
hasDivWithId			= hasNameWithId "div"

getMeta                         :: ArrowXml a => a XmlTree XmlTree
getMeta                         = getByPath ["html", "head", "meta"]

getLink                         :: ArrowXml a => a XmlTree XmlTree
getLink                         = getByPath ["html", "head", "link"]

getMetaAttr                     :: ArrowXml a => String -> a XmlTree String
getMetaAttr key                 = getMeta
                                  >>>
                                  hasAttrValue "name" ((== key) . map toLower)
                                  >>>
                                  getAttrValue "content"

-- ------------------------------------------------------------
--
-- text predicates


-- all strings with length < 2 are boring
-- and all strings not starting with a letter

boringWord              	:: String -> Bool
boringWord w            	= null w
                                  ||
                                  (null . tail $ w)
				  ||
				  not (any isXmlLetter w)

boringURIpart                   :: String -> Bool
boringURIpart                   = ( `elem`
                                    [ ""
                                    , "http", "www", "wwwab", "fh-wedel", "ptl", "de"
                                    , "html", "pdf"
                                    ]
                                  )

-- ------------------------------------------------------------
--
-- text preprocessing

deleteNotAllowedChars  		:: String -> String
deleteNotAllowedChars   	= map notAllowedToSpace
    where
    notAllowedToSpace c
        | isAllowedWordChar c   = c
        | otherwise             = ' '

isAllowedWordChar       	:: Char -> Bool
isAllowedWordChar c     	= isXmlLetter c
				  ||
				  isXmlDigit c
				  ||
				  c `elem` "_-"

-- ------------------------------------------------------------
--
-- tokenizing

uri2Words			:: String -> [String]
uri2Words			= tokenize "[^:/#?=.]+"

tokenizeDates                   :: String -> [String]
tokenizeDates                   = const []	-- not yet implemented

-- ------------------------------------------------------------
--
-- word normalization and translation

classifyURIword                 :: [String] -> [String]
classifyURIword                 = concatMap classifyWord
    where
    classifyWord w              = fromMaybe [] . lookup w $ table
    table                       = [ ("~rb", ["bwl", "wing", "winf"])
                                  , ("~eg", ["minf", "minf", "minf", "inf", "winf"])
                                  , ("~ur", ["minf", "winf", "inf"])
                                  , ("~wol", ["ia", "winf","stundenplan"])
                                  , ("ptl", ["ia", "ea", "pa", "iaw", "iam", "iat"])
                                  ]

short2Name			:: String -> String
short2Name sn                   = fromMaybe "" . lookup (drop 1 sn) $
                                  [ ("rb", "Ulrich Raubach")
                                  , ("ur", "Sven Urbanski")
                                  , ("eg", "Martin Egge")
                                  , ("si", "Uwe Schmidt")
                                  ]

normalizeDateModified		:: String -> String
normalizeDateModified           = id		-- not yet implemented

unwordsCont                     :: Int -> [String] -> String
unwordsCont mx ws
    | length ws' > mx           = unwords . (++ ["..."]) . init $ ws'
    | otherwise                 = unwords                       $ ws'
    where
    ws'                         = take (mx + 1) ws

-- ------------------------------------------------------------
