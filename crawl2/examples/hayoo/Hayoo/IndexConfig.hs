{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.IndexConfig
where

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
				  , ixName
				  , ixPartial
				  ]
    where
    ixDefault                   =  IndexContextConfig
                                   { ixc_name           = "default"
                                   , ixc_collectText    = getHtmlPlainText
                                   , ixc_textToWords    = deleteNotAllowedChars >>> words
                                   , ixc_boringWord     = boringWord
                                   }
    ixModule              	= ixDefault
                                  { ixc_name          	= "module"
                                  , ixc_collectText   	= getAttrValue "module"
				  , ixc_textToWords	= (:[])
                                  }
    ixHierachy			= ixModule
                                  { ixc_name          	= "hierarchy"
				  , ixc_textToWords	= tokenize "[^.]+"		-- split module name at .
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

stripSignature 			:: String -> String
stripSignature 			= tokenize "[()\\[\\],.]|::|=>|->|[A-Za-z0-9_#]+|[^()\\[\\],. A-Za-z0-9_#]+"
				  >>>
				  unwords
				  >>>
				  sed (take 1) "[(\\[][ ]"
				  >>>
				  sed (drop 1) "[ ][\\]),]"

-- -----------------------------------------------------------------------------    
{-
                        

theHayooXPath :: String
theHayooXPath =  "//td[@class='decl']"     
  
ccs_Hayoo :: [ContextConfig]
ccs_Hayoo = [ 
            , ccHayooSignature 
            , ccHayooNormalizedSignature
            , ccHayooDescription
            , ccPackage
            ]
            
ccHayooName :: ContextConfig
ccHayooName 
  = ContextConfig { cc_name        = "name"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees theHayooXPath  
                  , cc_fTokenize   = (\a -> [stripWith (\b -> b `elem` "():") (strip (takeWhile ( (/=) ' ') (dropWhile ( (==) ' ') a)))])
                  , cc_fIsStopWord = (flip elem) ["type", "class", "data"]  
                  , cc_addToCache  = False
                  }

ccHayooPartialName :: ContextConfig
ccHayooPartialName 
  = ccHayooName   { cc_name        = "partial"
                  , cc_fTokenize   = (\s -> split " " (deCamel False (takeWhile ( (/=) ' ') s))) 
                  } 
  where 
    deCamel :: Bool -> String -> String  -- Bool flag: last character was a capital letter
    deCamel _ []     = []
    deCamel wasCap (x:xs) 
      = if x == '_' then ' ' : deCamel True xs
                    else if x `elem` ['A'..'Z'] then if wasCap then x: deCamel True xs
                                                               else ' ' : (x : deCamel True xs) 
                                                else x : deCamel (x `elem` ['A'..'Z']) xs

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

ccHayooSignature :: ContextConfig
ccHayooSignature  
  = ContextConfig { cc_name        = "signature"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees theHayooXPath
                  , cc_fTokenize   = \s -> [getSignature s]
                  , cc_fIsStopWord = const False
                  , cc_addToCache  = False     
                  }

ccHayooNormalizedSignature :: ContextConfig
ccHayooNormalizedSignature 
  = ccHayooSignature { cc_name        = "normalized"
                     , cc_fTokenize   = (\s -> [normalizeSignature (getSignature s)])
                     , cc_fIsStopWord = (\s -> length s ==  0) 
                     }
                  
ccModule :: ContextConfig
ccModule 
  = ContextConfig { cc_name        = "module"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_fExtract    = getXPathTrees "/table/tr/td[@id='module']" 
                  , cc_fTokenize   = \a -> [stripWith (==' ') a]
                  , cc_fIsStopWord = const False     
                  , cc_addToCache  = False
                  }

ccHierarchy :: ContextConfig
ccHierarchy 
  = ccModule      { cc_name        = "hierarchy" 
                  , cc_fTokenize   = split "." . stripWith (==' ') 
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
