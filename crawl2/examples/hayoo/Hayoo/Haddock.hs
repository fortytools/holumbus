module Hayoo.Haddock
where

import		 Data.List
import		 Data.Maybe

import           Hayoo.URIConfig
import           Hayoo.FunctionInfo
import		 Hayoo.Signature

import           Holumbus.Utility

import           Network.URI		( unEscapeString )

import           Text.XML.HXT.Arrow     
import           Text.XML.HXT.Arrow.XmlRegex
import           Text.XML.HXT.XPath

-- ------------------------------------------------------------

hayooGetFctInfo			:: IOSArrow XmlTree FunctionInfo
hayooGetFctInfo			= ( getAttrValue "module"
				    &&&
				    getAttrValue "signature"
				    &&&
				    getAttrValue "package"
				    &&&
				    getAttrValue "source"
				  )
				  >>^
				  (\ (m, (s, (p, r))) -> mkFunctionInfo m s p r)

hayooGetTitle			:: IOSArrow XmlTree String
hayooGetTitle			= fromLA $
				  getAttrValue "module"

-- ------------------------------------------------------------

prepareHaddock			:: IOSArrow XmlTree XmlTree
prepareHaddock			= process
                                  [ this
                                  , isHaddock
                                  , addPackageAttr
                                  , addModuleAttr
                                  , processClasses				-- .4
                                  , topdeclToDecl 				-- .5
                                  , removeDataDocumentation			-- .6
                                  , processDataTypeAndNewtypeDeclarations	-- .7
                                  , processCrazySignatures
                                  , splitHaddock
                                  ]
    where
    process			= seqA . zipWith phase [(0::Int)..]
    phase _i f			= fromLA f
                                  -- >>>
                                  -- traceDoc ("prepare haddock: step " ++ show i)

-- ------------------------------------------------------------

isHaddock			:: LA XmlTree XmlTree
isHaddock			= ( getPath "html/body/table/tr"
                                    /> isElemWithAttr "td" "class" (== "botbar")
                                    />  hasName "a"
                                    />  hasText (== "Haddock")
                                  ) `guards` this

getTitle			:: LA XmlTree String
getTitle			= xshow
                                  ( getPath "html/head/title"
                                    >>> deep isText
                                  )

getPackage			:: LA XmlTree String
getPackage			= getAttrValue transferURI
                  	          >>^	
                                  hayooGetPackage

addPackageAttr			:: LA XmlTree XmlTree
addPackageAttr			= this += (attr "package" $ getPackage >>> mkText)

addModuleAttr			:: LA XmlTree XmlTree
addModuleAttr			= this += ( attr "module" $ getTitle   >>> mkText)

-- ------------------------------------------------------------

splitHaddock			:: LA XmlTree XmlTree
splitHaddock			= mkVirtualDoc $< this

mkVirtualDoc 			:: XmlTree -> LA XmlTree XmlTree
mkVirtualDoc rt			= getDecls
                                  >>>
                                  ( root [] []
                                    += attr "title"     (theTitle     >>> mkText)
                                    += attr "module"    (theModule    >>> mkText)
                                    += attr "package"   (thePackage   >>> mkText)
                                    += attr "signature" (theSignature >>> mkText)
                                    += attr "source"    (theSourceURI >>> mkText)
                                    += attr transferURI ( ( (theURI &&& theLinkPrefix &&& theTitle)
                                                            >>^
                                                            (\ (u, (h, t)) -> u ++ h ++ t)
                                                          )
                                                          >>>
                                                          mkText
                                                        )
                                    += removeSourceLinks
                                  )
    where
    getDecls			= deep ( isDecl' >>> hasAttr "id" )
    isDecl'			= isElemWithAttr "tr" "class" (== "decl")
    theTitle			= ( listA (isDecl' >>> getAttrValue "id") >>. concat )
                                  >>^
                                  unEscapeString

    theSignature		= xshow ( removeSourceLinks
                                          >>>
                                          deep (isElemWithAttr "td" "class" (== "decl"))
                                          >>>
                                          deep isText
                                        )
                                  >>^
                                  getSignature

    theLinkPrefix		= theSignature
                                  >>^ (\ s -> if s `elem` ["data", "type", "newtype"]
                                              then "#t:"
                                              else "#v:"
                                      )
    theSourceURI		= ( single
                                    ( ( deep ( ( isElemWithAttr "a" "href" ("src/" `isPrefixOf`)
                                                 />
                                                 hasText (== "Source")
                                               )
                                               `guards` this
                                             )
                                        >>>
                                        getAttrValue0 "href"
                                      )
                                      &&&
                                      theURI
                                    )
                                    >>^ (uncurry expandURIString >>> fromMaybe "")
                                  )
                                  `withDefault` ""

    theModule			= constA rt >>> getAttrValue "module"
    thePackage			= constA rt >>> getAttrValue "package"
    theURI			= constA rt >>> getAttrValue transferURI

-- ------------------------------------------------------------
 
-- | Transform classes so that the methods are wrapped into the same html as normal functions

processClasses 			:: LA XmlTree XmlTree
processClasses 			= processTopDown
                                  ( processClassMethods
                                    `when`
                                    ( getClassPart "section4"
                                      /> hasText (== "Methods")
                                    )
                                  )
    where   
    processClassMethods 	= getClassPart "body"
                                  /> hasName "table"
                                  /> hasName "tr"
                                -- getXPathTrees "/tr/td[@class='body']/table/tr/td[@class='body']/table/tr" 

-- ------------------------------------------------------------

-- | Removes Source Links from the XmlTree. A Source Link can be identified by the text of an "a" 
--   node but to be more precise it is also checked whether the href-attribute starts with "src".
--   During the tree transformation it might happen, that source links with empty href attributes 
--   are constructed so empty href attributes are also searched and removed if the text of the "a"
--   node is "Source"

removeSourceLinks 		:: LA XmlTree XmlTree
removeSourceLinks 		= processTopDown
                                  ( none
                                    `when` 
                                    ( isElemWithAttr "a" "href" (\a -> null a || "src/" `isPrefixOf` a)
                                      />
                                      hasText (== "Source")
                                    )
                                  )      

-- ------------------------------------------------------------

-- | As Haddock can generate Documentation pages with links to source files and without these links
--   there are two different types of declaration table datas. To make the indexing easier, the
--   table datas with source links are transformed to look like those without (they differ 
--   in the css class of the table data and the ones with the source links contain another table).

topdeclToDecl 			:: LA XmlTree XmlTree
topdeclToDecl  			= processTopDownUntil
                                  ( isElemWithAttr "table" "class" (== "declbar")
                                   `guards`
                                   ( getChildren >>> getChildren >>> getChildren )
                                  )
                                  >>>
                                  processTopDownUntil
                                  ( isElemWithAttr "td" "class" (== "topdecl")
                                    `guards`
                                    mkelem "td" [ sattr "class" "decl"] [ getChildren ]
                                  ) 

-- ------------------------------------------------------------

removeDataDocumentation		:: LA XmlTree XmlTree
removeDataDocumentation 	= processTopDown 
                                  ( none
                                    `when`
                                    ( getClassPart "section4"
                                      /> hasText (\a -> a == "Constructors"
                                                        || "Instances" `isSuffixOf` a
                                                 )
                                    )
                                  )

-- ------------------------------------------------------------

processDataTypeAndNewtypeDeclarations :: LA XmlTree XmlTree
processDataTypeAndNewtypeDeclarations 
  				= processTopDownUntil
                                  ( (    isElemWithAttr "td"   "class" (=="decl")
                                      /> isElemWithAttr "span" "class" (=="keyword")
                                      /> hasText (`elem` ["data", "type", "newtype", "class"]) 
                                    )
                                    `guards`
                                    ( mkTheElem $<<<< (     getTheName
                                                        &&& getTheType
                                                        &&& getTheRef
                                                        &&& getTheSrc
                                                      )
                                    ) 
                                  )
    where
    getTheName 			= xshow $
                                  deep (hasName "b") /> isText

    getTheRef  			= ( single $
                                    deep (hasName "a" >>> getAttrValue0 "name")
                                  )
                                  `withDefault` ""

    getTheType 			= xshow $
                                  single $
                                  deep (isElemWithAttr "span" "class" (== "keyword")) /> isText

    getTheSrc			= ( single $
                                    deep (isElemWithAttr "a" "href" ("src/" `isPrefixOf`))
                                    >>>
                                    getAttrValue0 "href"
                                  )
                                  `withDefault` ""

    mkTheElem n t r s 		= eelem "td"
                                  +=   sattr "class" "decl"
                                  += ( eelem "a"
                                       +=   sattr "name" r
                                       +=   txt (n ++ " :: " ++ t)
                                     )
                                  += ( eelem "a"
                                       += sattr "href" s
                                       += txt "Source"
                                     )

-- ------------------------------------------------------------

processCrazySignatures 		:: LA XmlTree XmlTree
processCrazySignatures		= processTopDown
                                  ( preProcessCrazySignature
                                    `when`
                                    getClassPart "rdoc"
                                  )
                                  >>>
                                  processChildren
                                  ( processDocumentRootElement groupDeclSig declAndDocAndSignatureChildren )

preProcessCrazySignature	:: LA XmlTree XmlTree
preProcessCrazySignature	= ( selem "tr" 
                                    [ mkelem "td" [ sattr "class" "signature" ]
                                                  [ deep (isElemWithAttr "td" "class" (== "arg"))
                                                    >>>
                                                    getChildren
                                                  ]  
                                    ] 
                                    &&&   
                                    selem "tr" 
                                    [ mkelem "td" [ sattr "class" "doc" ]
                                                  [ deep (isElemWithAttr "td" "class" (== "rdoc"))
                                                    >>>
                                                    getChildren
                                                  ]
                                    ]
                                  )
                                  >>> mergeA (<+>)
         
processDocumentRootElement 	:: (LA XmlTree XmlTree -> LA XmlTree XmlTree)
                                ->  LA XmlTree XmlTree
                                ->  LA XmlTree XmlTree
processDocumentRootElement theGrouper interestingChildren
    				= processTopDownUntil
                                  ( hasName "table"
                                    `guards`
                                    ( replaceChildren
                                      ( processTableRows theGrouper (getChildren >>> interestingChildren) )
                                    )
                                  )
           
declAndDocChildren  		:: LA XmlTree XmlTree
declAndDocChildren  		= (isDecl <+> isDoc) `guards` this

declAndDocAndSignatureChildren :: LA XmlTree XmlTree
declAndDocAndSignatureChildren = (isDecl <+> isSig <+> isDoc) `guards` this

isDecl      			:: LA XmlTree XmlTree
isDecl				= hasTDClass (== "decl")
                                  />
                                  isElemWithAttr "a" "name" (const True)

isDoc     			:: LA XmlTree XmlTree
isDoc				= hasTDClass (== "doc")
      
isSig    			:: LA XmlTree XmlTree
isSig				= hasTDClass (== "signature")

isArg 				:: LA XmlTree XmlTree
isArg 				= hasTDClass (== "arg")

getDeclName     		:: LA XmlTree String
getDeclName			= (xshow $ single $ getXPathTrees "//tr/td/a/@name/text()") >>^ drop 2

processTableRows      		:: (LA XmlTree XmlTree -> LA XmlTree XmlTree)
                                ->  LA XmlTree XmlTree
                                ->  LA XmlTree XmlTree
processTableRows theGrouping ts	= theGrouping (remLeadingDocElems ts) {- >>> prune 3 -}

-- regex for a leading class="doc" row

leadingDoc    			:: XmlRegex
leadingDoc    			= mkStar (mkPrimA isDoc)

-- regex for a class="decl" class="doc" sequence

declDoc     			:: XmlRegex
declDoc     			= mkSeq (mkPrimA isDecl) leadingDoc

declSig     			:: XmlRegex
declSig     			= mkSeq (mkPrimA isDecl) (mkSeq (mkStar (mkPrimA isSig)) leadingDoc)

-- remove a leading class="doc" row this does not form a declaration
-- split the list of trees and throw away the first part

remLeadingDocElems  		:: LA XmlTree XmlTree -> LA XmlTree XmlTree
remLeadingDocElems ts   	= (splitRegexA leadingDoc ts >>^ snd) >>> unlistA

-- group the "tr" trees for a declaration together, build a "tr class="decl"" element and
-- throw the old "tr" s away

groupDeclSig    		:: LA XmlTree XmlTree -> LA XmlTree XmlTree
groupDeclSig ts 		= scanRegexA declSig ts 
                                  >>>
                                  mkelem  "tr"
                                  [ sattr "class" "decl"
                                  , attr "id" (unlistA >>> getDeclName >>> mkText)
                                  ] 
                                  [ mkelem "td" [sattr "class" "decl"] 
                                                [unlistA
                                                 >>> getXPathTrees "//td[@class='decl' or @class='signature']"
                                                 >>> getChildren
                                                ] 
                                  , mkelem "td" [sattr "class" "doc" ]
                                                [unlistA
                                                 >>> getXPathTrees "//td[@class='doc']"
                                                 >>> getChildren
                                                ] 
                                  ]


removeSpacers 			:: LA XmlTree XmlTree
removeSpacers 			= processTopDown
                                  ( none
                                    `when`
                                    hasTDClass (`elem` ["s15", "s8"])
                                  )

-- ------------------------------------------------------------

getPath				:: String -> LA XmlTree XmlTree
getPath				= foldl (/>) this . map hasName . split "/"

isElemWithAttr			:: String -> String -> (String -> Bool) -> LA XmlTree XmlTree
isElemWithAttr en an av		= isElem
                                  >>>
                                  hasName en
                                  >>>
                                  hasAttrValue an av

hasTDClass			:: (String -> Bool) -> LA XmlTree XmlTree
hasTDClass av			= hasName "tr"
                                  />
                                  isElemWithAttr "td" "class" av

getClassPart			:: String -> LA XmlTree XmlTree
getClassPart c			= hasTDClass (== "body")
                                  /> hasName "table"
                                  /> hasTDClass (== c)

-- ------------------------------------------------------------

