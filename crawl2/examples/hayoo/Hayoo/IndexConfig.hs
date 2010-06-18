{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.IndexConfig
where

import           Hayoo.Haddock
import           Hayoo.HackagePackage
import           Hayoo.Signature

import		 Holumbus.Crawler
import		 Holumbus.Crawler.IndexerCore

import		 Text.XML.HXT.Arrow
import		 Text.XML.HXT.DOM.Unicode

-- ------------------------------------------------------------

hayooIndexContextConfig		:: [IndexContextConfig]
hayooIndexContextConfig		= [ ixModule
				  , ixHierachy
                                  , ixPackage
				  , ixName
				  , ixPartial
                                  , ixSignature
                                  , ixNormalizedSig
				  , ixDescription
				  ]
    where
    ixDefault                   = IndexContextConfig
                                  { ixc_name           	= "default"
                                  , ixc_collectText    	= getHtmlPlainText
                                  , ixc_textToWords    	= deleteNotAllowedChars >>> words
                                  , ixc_boringWord     	= boringWord
                                  }
    ixModule              	= ixDefault
                                  { ixc_name          	= "module"
                                  , ixc_collectText   	= getAttrValue "module"
				  , ixc_textToWords	= return
                                  , ixc_boringWord     	= null
                                  }
    ixHierachy			= ixModule
                                  { ixc_name          	= "hierarchy"
				  , ixc_textToWords	= tokenize "[^.]+"		-- split module name at .
                                  }
    ixPackage              	= ixDefault
                                  { ixc_name          	= "package"
                                  , ixc_collectText   	= getAttrValue "package"
				  , ixc_textToWords	= return
                                  , ixc_boringWord	= (== "unknownpackage")
                                  }
    ixName			= ixDefault
				  { ixc_name          	= "name"
                                  , ixc_collectText   	= fromLA $ getAllText (deep $ hasTDClass (== "decl"))
				  , ixc_textToWords	= tokenize "[^ ():]+" >>> take 1
				  , ixc_boringWord	= (`elem` ["type", "class", "data"])
                                  }
    ixPartial			= ixName
                                  { ixc_name          	= "partial"
				  , ixc_textToWords	= deCamel >>> tokenize typeIdent
                                  , ixc_boringWord      = boringWord
                                  }
    ixSignature			= ixDefault
				  { ixc_name          	= "signature"
                                  , ixc_collectText   	= getAttrValue "signature"
				  , ixc_textToWords	= stripSignature >>> return
				  , ixc_boringWord	= not . isSignature
                                  }
    ixNormalizedSig		= ixSignature
				  { ixc_name          	= "normalized"
				  , ixc_textToWords	= normalizeSignature >>> return
                                  }
    ixDescription              	= ixDefault
                                  { ixc_name          	= "description"
                                  , ixc_collectText   	= fromLA $ getAllText (deep $ hasTDClass (== "doc"))
				  , ixc_textToWords	= tokenize descrWord
                                  }

-- -----------------------------------------------------------------------------    

hayooPkgIndexContextConfig	:: [IndexContextConfig]
hayooPkgIndexContextConfig	= [ ixCategory
                                  , ixPkgName
				  , ixDepends
				  , ixDescription
                                  , ixSynopsis
                                  , ixAuthor
				  ]
    where
    ixDefault                   = IndexContextConfig
                                  { ixc_name           	= "default"
                                  , ixc_collectText    	= getHtmlPlainText
                                  , ixc_textToWords    	= deleteNotAllowedChars >>> words
                                  , ixc_boringWord     	= boringWord
                                  }
    ixCategory              	= ixDefault
                                  { ixc_name          	= "category"
                                  , ixc_collectText   	= fromLA getPkgCategory
				  , ixc_textToWords	= words
                                  }
    ixPkgName              	= ixDefault
                                  { ixc_name          	= "pkgname"
                                  , ixc_collectText   	= fromLA $ getPkgName
				  , ixc_textToWords	= return
                                  }
    ixDepends              	= ixDefault
                                  { ixc_name          	= "dependencies"
                                  , ixc_collectText   	= fromLA $ getPkgDependencies >>^ unwords
				  , ixc_textToWords	= words
                                  }
    ixDescription              	= ixDefault
                                  { ixc_name          	= "pkgdescr"
                                  , ixc_collectText   	= fromLA $ getPkgDescr
				  , ixc_textToWords	= tokenize descrWord
                                  }
    ixSynopsis              	= ixDescription
                                  { ixc_name          	= "synopsis"
                                  , ixc_collectText   	= fromLA $ getPkgSynopsis
                                  }
    ixAuthor              	= ixDescription
                                  { ixc_name          	= "author"
                                  , ixc_collectText   	= fromLA $
                                                          listA (getPkgAuthor <+> getPkgMaintainer) >>^ unwords
                                  }

-- -----------------------------------------------------------------------------    

-- please: "-" as 1. char in set !!!
-- words start with a letter, end with a letter or digit and may contain -, . and @ and digits

descrWord			:: String
descrWord			= "[A-Za-z][-A-Za-z0-9.@]*[A-Za-z0-9]"

-- -----------------------------------------------------------------------------    

deCamel				:: String -> String
deCamel				= deCamel' False
    where
    deCamel' _ []     		= []
    deCamel' _ ('_' : xs)	= ' ' : deCamel' True xs
    deCamel' cap (x : xs)
	| isCap x		= ( if cap
				    then id
				    else (' ' :)
				  ) $
                                  x : deCamel' True      xs
	| otherwise		= x : deCamel' (isCap x) xs
	where
	isCap			= (`elem` ['A'..'Z'])

-- -----------------------------------------------------------------------------    

boringWord              	:: String -> Bool
boringWord w            	= null w
                                  ||
                                  (null . tail $ w)
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

-- -----------------------------------------------------------------------------    
