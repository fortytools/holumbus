{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.IndexConfig
where

import           Data.Maybe

import		 Holumbus.Crawler
import		 Holumbus.Crawler.IndexerCore

import		 Text.XML.HXT.Arrow
import		 Text.XML.HXT.DOM.Unicode
import		 Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

import           Hayoo.Haddock

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
				  , ixc_textToWords	= deCamel >>> tokenize "[A-Za-z][A-Za-z0-9_]*"
                                  }
    ixSignature			= ixDefault
				  { ixc_name          	= "signature"
                                  , ixc_collectText   	= getAttrValue "signature"
				  , ixc_textToWords	= stripSignature >>> return
				  , ixc_boringWord	= null
                                  }
    ixNormalizedSig		= ixSignature
				  { ixc_name          	= "normalized"
				  , ixc_textToWords	= normSignature >>> return
                                  }
    ixDescription              	= ixDefault
                                  { ixc_name          	= "description"
                                  , ixc_collectText   	= fromLA $ getAllText (deep $ hasTDClass (== "doc"))
				  , ixc_textToWords	= tokenize "[A-Za-z0-9.-_'():]+"
                                  }

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

stripSignature'			:: ([String] -> [String]) -> String -> String
stripSignature' nf		= tokenize "[()\\[\\],.]|::|=>|->|[A-Za-z0-9_#'.xdg]+|[^()\\[\\],. A-Za-z0-9_#']+"
				  >>>
                                  nf
                                  >>>
				  unwords
				  >>>
				  sed init  "([,()\\[\\]]|->)[ ]"
				  >>>
				  sed (drop 1) "[ ]([\\[\\](),]|->)"

stripSignature			:: String -> String
stripSignature			= stripSignature' id

normSignature			:: String -> String
normSignature			= stripSignature' normIds

normIds				:: [String] -> [String]
normIds	ts			= map renId ts
    where
    ids				= flip zip (map (:[]) ['a' ..]) . filter isId $ ts
    isId			= (`elem` (['A'..'Z'] ++ ['a'..'z'] ++ "#_")) . head
    renId t			= fromMaybe t . lookup t $ ids

-- -----------------------------------------------------------------------------    
{-
                        

theHayooXPath :: String
theHayooXPath =  "//td[@class='decl']"     
  
ccs_Hayoo :: [ContextConfig]
ccs_Hayoo = [ 
            , ccHayooDescription
            , ccPackage
            ]
            

-- | Configruation for description context.
ccHayooDescription :: ContextConfig 
ccHayooDescription 
  = ContextConfig { cc_name        = "description" 
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees "//td[@class='doc']"
                  , cc_fTokenize   = map (stripWith (=='.')) . 
                                         (parseWords (\c -> isAlphaNum c || c `elem` ".-_'@():"))
--                  , cc_fTokenize   = map (stripWith (=='.')) . (parseWords isWordChar)
                  , cc_fIsStopWord = \s -> length s < 2
                  , cc_addToCache  = True
                  }

ccPackage :: ContextConfig
ccPackage
  = ContextConfig { cc_name        = "package"
                  , cc_preFilter   = this
                  , cc_fExtract    = getXPathTrees "/table/tr/td[@id='package']"
                  , cc_fTokenize   = \a -> [stripWith (==' ') a]
                  , cc_fIsStopWord = (flip elem) ["unknownpackage"]
                  , cc_addToCache  = False
                  }
                              
getSignature :: String -> String
getSignature s = stripWith (==' ') $ 
                  if "=>" `isInfixOf` s  then stripSignature $ drop 3 $ dropWhile ((/=) '=') s
                                         else stripSignature $ drop 3 $ dropWhile ((/=) ':') s


-- | TODO some of this is redundant, some important. find out what and why and simplify
preFilterSignatures :: LA XmlTree XmlTree
preFilterSignatures = this
--      >>> topDeclToDecl
      >>> flattenElementsByType "a"
      >>> flattenElementsByType "b"
      >>> flattenElementsByType "span"
      >>> flattenElementsByType "em"
      >>> flattenElementsByType "p"
      >>> flattenElementsByType "pre" 
--      >>> removeDeclbut


flattenElementsByType :: String -> LA XmlTree XmlTree
flattenElementsByType s = processTopDown ( getChildren `when` hasName s )

removeDeclbut :: ArrowXml a => a XmlTree XmlTree
removeDeclbut = processTopDown (  none `when` isDeclbut )
  where isDeclbut = isElem  >>> hasName "td" >>> hasAttrValue "class" (isPrefixOf "declbut") 

-}
-- ------------------------------------------------------------

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

-- ------------------------------------------------------------
