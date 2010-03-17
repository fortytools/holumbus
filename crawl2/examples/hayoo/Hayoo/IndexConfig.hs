{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.IndexConfig
where

import		 Holumbus.Crawler
import		 Holumbus.Crawler.IndexerCore

import		 Text.XML.HXT.Arrow
import		 Text.XML.HXT.DOM.Unicode
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch()

-- ------------------------------------------------------------

hayooIndexContextConfig		:: [IndexContextConfig]
hayooIndexContextConfig		= ixc
    where
    ixDefault                   =  IndexContextConfig
                                   { ixc_name           = "default"
                                   , ixc_collectText    = getHtmlPlainText
                                   , ixc_textToWords    = deleteNotAllowedChars >>> words
                                   , ixc_boringWord     = boringWord
                                   }
    ixc                         = [{- ixDefault
                                    { ixc_name          = "title"
                                    , ixc_collectText   = getHtmlTitle
                                    }
                                  , ixDefault
                                    { ixc_name          = "headlines"
                                    , ixc_collectText   = getAllText getHeadlines
                                    }
                                  , -} ixDefault
                                    { ixc_name          = "content"
                                    , ixc_collectText   = getAllText this
                                    }
                                  ]

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
