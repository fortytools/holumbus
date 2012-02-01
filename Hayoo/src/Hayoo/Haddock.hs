{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.Haddock
    ( hayooGetFctInfo
    , hayooGetTitle
    , hayooGetDescr
    , prepareHaddock
    )
where

import Data.List
import Data.Maybe

import Hayoo.URIConfig
import Hayoo.FunctionInfo
import Hayoo.Signature

import Holumbus.Crawler.Html
import Holumbus.Utility

import Network.URI                      ( unEscapeString )

import Text.Regex.XMLSchema.String      ( match
                                        , tokenize
                                        )

import Text.XML.HXT.Core     
import Text.XML.HXT.Arrow.XmlRegex
import Text.XML.HXT.XPath

-- ------------------------------------------------------------

hayooGetFctInfo                 :: IOSArrow XmlTree FunctionInfo
hayooGetFctInfo                 = fromLA $
                                  ( getAttrValue "module"
                                    &&&
                                    getAttrValue "signature"
                                    &&&
                                    getAttrValue "package"
                                    &&&
                                    getAttrValue "source"
                                    &&&
                                    xshow
                                    ( hayooGetDescr
                                      >>>
                                      getChildren
                                      >>>
                                      editDescrMarkup
                                    )
                                  )
                                  >>^
                                  (\ (m, (s, (p, (r, d)))) -> mkFunctionInfo m s p r d)

hayooGetTitle                   :: IOSArrow XmlTree String
hayooGetTitle                   = fromLA $
                                  getAttrValue "title"

hayooGetDescr                   :: LA XmlTree XmlTree
hayooGetDescr                   = ifA version28
                                  ( getChildren                                 -- 2.8
                                    >>>
                                    divWithClass (== "top")
                                    >>>
                                    firstChildWithClass "div" "doc"
                                  )
                                  ( deep (trtdWithClass (== "doc")) )           -- 2.6

editDescrMarkup                 :: LA XmlTree XmlTree
editDescrMarkup                 = processTopDown
                                  ( remHref `when` hasName "a" )
    where
    remHref                     = processAttrl (none `when` hasName "href")

version28                       :: LA XmlTree XmlTree
version28                       = hasAttrValue "version" (== "2.8")

-- ------------------------------------------------------------

prepareHaddock                  :: IOSArrow XmlTree XmlTree
prepareHaddock                  = ( traceMsg 1 "prepareHaddock: try version 2.8"
                                    >>>
                                    prepareHaddock28
                                  )
                                  `orElse`
                                  ( traceMsg 1 "prepareHaddock: try version 2.6"
                                    >>>
                                    prepareHaddock26
                                  )
                                  `orElse`
                                  ( traceMsg 1 "prepareHaddock: no haddock" )

-- ------------------------------------------------------------

isHaddock28                     :: LA XmlTree XmlTree
isHaddock28                     = getPath "html/body/div/p"
                                  >>>
                                  ( hasAElemWithHaddock
                                    `guards`
                                    hasVersionGE28
                                  )
    where
    hasAElemWithHaddock         = this
                                  /> hasName "a"
                                  /> hasText (== "Haddock")
    hasVersionGE28              = this
                                  /> hasText matchVersionGE28

matchVersionGE28                :: String -> Bool
matchVersionGE28                = match ".* [2-9][.]([1-9][0-9]+|[8-9])([.][0-9]+)*"

prepareHaddock28                :: IOSArrow XmlTree XmlTree
prepareHaddock28                = fromLA $
                                  seqA
                                  [ this
                                  , ( isHaddock28 `guards` this )
                                  , addPackageAttr
                                  , addModuleAttr
                                  , splitHaddock28
                                  ]
                                  -- >>> withTraceLevel 4 (traceDoc "result of splitHaddock28") -- just for dev.

splitHaddock28                  :: LA XmlTree XmlTree
splitHaddock28                  = mkVirtualDoc28 $< this

mkVirtualDoc28                  :: XmlTree -> LA XmlTree XmlTree
mkVirtualDoc28 rt               = (getModule <+> getDecls)
                                  >>>
                                  mkDoc
    where
    mkDoc                       = ( root [] []
                                    += attr "title"     (theTitle     >>> mkText)
                                    += attr "module"    (theModule    >>> mkText)
                                    += attr "package"   (thePackage   >>> mkText)
                                    += attr "signature" (theSignature >>> mkText)
                                    += attr "source"    (theSourceURI >>> mkText)
                                    += sattr "version"  "2.8"
                                    += attr transferURI ( ( (theURI &&& theLinkPrefix &&& theTitle)
                                                            >>^
                                                            (\ (u, (h, t)) -> u ++ h ++ t)
                                                          )
                                                          >>>
                                                          mkText
                                                        )
                                    += removeSourceLinks
                                  )

    getModule                   =  mkModuleDecl
                                   ( ( eelem "a"
                                       += sattr "class" "def"
                                       += theModuleName
                                     )
                                     <+>
                                     theSourceLink
                                   )
                                   theModuleDoc
        where
        theModuleName           = xshow
                                  ( getById "module-header"                     -- single (deep (isElemWithAttr "div" "id" (== "module-header")))
                                    /> pWithClass (== "caption")
                                    /> isText
                                  )
                                  >>>
                                  arr (tokenize "[^.]+" >>> last)                -- A will be the name of module C.B.A
                                  >>>
                                  mkText

        theSourceLink           = getById "package-header"                      -- single (deep (isElemWithAttr "div" "id" (== "package-header")))
                                  /> getById "page-menu"                        -- isElemWithAttr "ul" "id" (== "page-menu")
                                  /> hasName "li"
                                  /> hasName "a"
                                  >>> ( (getChildren >>> hasText (== "Source"))
                                        `guards`
                                        ( this += sattr "class" "link" )
                                      )

        theModuleDoc            = getById "description"                         -- single (deep (isElemWithAttr "div" "id" (== "description")))
                                  />
                                  divWithClass (== "doc")

    getDecls                    = getById "interface"                           -- this //> isElemWithAttr "div" "id" (== "interface")
                                  />  divWithClass (== "top")
                                  >>> choiceA
                                      [ isDataTypeNewtypeDecl
                                                       :-> ( processTypeDecl
                                                             <+>
                                                             ( processConstructors $< getSrcLnk ) -- the data sourrce link is propagated
                                                           )                                      -- to the constructors and fields
                                      , isClassDecl    :-> ( processClassDecl
                                                             <+>
                                                             processMethods
                                                           )
                                      , isFunctionDecl :-> processFunctionDiv
                                      , this           :-> none
                                      ]

    getSrcLnk                   = ( first_p_src
                                    >>>
                                    firstChildWithClass "a" "link"
                                  )
                                  `orElse`
                                  txt ""

    isFunctionDecl              = first_p_src
                                  >>>
                                  firstChild (aWithClass (== "def")
                                              >>>
                                              hasAttr "name"
                                             )

    isClassDecl                 = isTDecl (== "class")

    isDataTypeNewtypeDecl       = isTDecl (`elem` ["data", "type", "newtype"])

    isTDecl p                   = first_p_src                                   -- check the first p element found
                                  >>>
                                  firstChildWithClass "span" "keyword"          -- check the first keyword found
                                  />
                                  hasText p

    processFunctionDiv         = (mkSingleDiv $< splitMultiDiv)
                                 += sattr "type" "function"
        where
          mkSingleDiv ts        = replaceChildren (constL ts)
          splitMultiDiv         = listA getChildren
                                  >>> arr (head &&& tail)
                                  >>> first processFunctionSig
                                  >>> arr (uncurry (:))

    processFunctionSig          = mkSingleFct $< splitMultiFct
        where
          mkSingleFct ts        = replaceChildren (constL ts)
          splitMultiFct         = listA getChildren
                                  >>> spanA (neg $ hasText ((== "::") . stringTrim))
                                  >>> first ( unlistA
                                              >>> aWithClass (== "def")
                                              >>> hasAttr "name"
                                            ) 
                                  >>> arr (uncurry (:))

    processTypeDecl             = this +=  attr "type" theType

    processConstructors srcLnk  = this
                                  />  divWithClass (words >>> ("constructors" `elem`))
                                  />  hasName "table"
                                  />  hasName "tr"
                                  >>> ( (isConstrRow `guards` theConstructors srcLnk)
                                        <+>
                                        processConstrFields srcLnk
                                      )
    isConstrRow                 = matchRegexA ( mkSeq
                                                (mkPrimA $ tdWithClass (== "src"))
                                                (mkPrimA $ tdWithClass (words >>> ("doc" `elem`)))
                                              )
                                  getChildren
                                  >>>
                                  unlistA

    processConstrFields srcLnk  = getChildren
                                  >>>
                                  hasName "td"
                                  />
                                  divWithClass (words >>> ("fields" `elem`))
                                  />
                                  hasName "dl"
                                  >>>
                                  scanRegexA 
                                      ( mkSeq (mkPrimA $ hasName "dt") (mkPrimA $ hasName "dd"))
                                      getChildren
                                  >>>
                                  theConstrFields srcLnk

    processClassDecl            = ( this += sattr "type" "class" )
                                  >>>
                                  processChildren ( pWithClass   (== "src")
                                                    <+>
                                                    divWithClass (== "doc")
                                                  )

    processMethods              = getChildren
                                  >>>
                                  divWithClass (words >>> ("methods" `elem`))
                                  >>>
                                  scanRegexA
                                    ( mkSeq
                                      (        mkPrimA $ hasName "p")
                                      (mkOpt $ mkPrimA $ hasName "div")
                                    )
                                    ( getChildren
                                      >>>
                                      ( pWithClass   (== "src")
                                        <+>
                                        divWithClass (words >>> ("doc" `elem`))
                                      )
                                    )
                                  >>>
                                  theMethods

    theType                     = single (getPath "p/span" /> isText)

    theTitle                    = xshow ( first_p_src
                                          >>>
                                          firstChildWithClass "a" "def"
                                          />
                                          isText
                                        )
                                  >>^
                                  unEscapeString

    theConstructors srcLnk      = mkFctDecl
                                  ( ( getChildren
                                      >>>
                                      tdWithClass (== "src")
                                      >>>
                                      getChildren
                                    )
                                    <+>
                                    constA srcLnk
                                  )
                                  ( getChildren
                                    >>>
                                    tdWithClass (== "doc")
                                    >>>
                                    getChildren
                                  )

    theConstrFields srcLnk      = mkFctDecl
                                  ( (unlistA >>> hasName "dt" >>> getChildren)
                                    <+>
                                    constA srcLnk
                                  )
                                  ( unlistA >>> hasName "dd" >>> getChildren )

    theMethods                  = mkDecl0 "function" ( this >>> unlistA )

    theLinkPrefix               = ( first_p_src
                                    >>>
                                    firstChildWithClass "a" "def"
                                    >>>
                                    getAttrValue "name"
                                    >>^
                                    (take 2 >>> ('#' :))
                                  )
                                  `withDefault` "#v:"

    theSignature                = ( ifA
                                    ( hasAttrValue "type" (`elem` ["function"]) )
                                    ( xshow ( ( single ( this /> pWithClass (== "src") )
                                                <+>
                                                theSubArguments         -- fancy arguments
                                              )
                                              >>>
                                              removeSourceLinks
                                              >>>
                                              deep isText
                                            )
                                    )
                                    ( getAttrValue "type" )
                                  )
                                  >>^
                                  getSignature

    theSubArguments             = getChildren
                                  >>>
                                  divWithClass (words >>> ("arguments" `elem`))
                                  />
                                  hasName "table"
                                  />
                                  hasName "tr"
                                  />
                                  tdWithClass (== "src")
                                  >>>
                                  getChildren
                                  
    theSourceURI                = ( first_p_src
                                    >>>
                                    firstChildWithClass "a" "link"
                                    >>>
                                    getAttrValue       "href"
                                  )
                                  `withDefault` ""

    theModule                   = constA rt >>> getAttrValue "module"
    thePackage                  = constA rt >>> getAttrValue "package"
    theURI                      = constA rt >>> getAttrValue transferURI

-- ------------------------------------------------------------

isHaddock26                     :: LA XmlTree XmlTree
isHaddock26                     = getPath "html/body/table/tr"
                                  /> tdWithClass (== "botbar")
                                  /> hasName "a"
                                  /> hasText (== "Haddock")

prepareHaddock26                :: IOSArrow XmlTree XmlTree
prepareHaddock26                = process
                                  [ this
                                  , ( isHaddock26 `guards` this )
                                  , addPackageAttr
                                  , addModuleAttr
                                  , processClasses                              -- .4
                                  , topdeclToDecl                               -- .5
                                  , removeDataDocumentation                     -- .6
                                  , processDataTypeAndNewtypeDeclarations       -- .7
                                  , processCrazySignatures
                                  , splitHaddock26
                                  ]
    where
    process                     = seqA . zipWith phase [(0::Int)..]
    phase _i f                  = fromLA f
                                  -- >>>
                                  -- traceDoc ("prepare haddock-2.6: step " ++ show i)

splitHaddock26                  :: LA XmlTree XmlTree
splitHaddock26                  = mkVirtualDoc26 $< this

mkVirtualDoc26                  :: XmlTree -> LA XmlTree XmlTree
mkVirtualDoc26 rt               = getDecls
                                  >>>
                                  ( root [] []
                                    += attr "title"     (theTitle     >>> mkText)
                                    += attr "module"    (theModule    >>> mkText)
                                    += attr "package"   (thePackage   >>> mkText)
                                    += attr "signature" (theSignature >>> mkText)
                                    += attr "source"    (theSourceURI >>> mkText)
                                    += sattr "version"  "2.6"
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
    getDecls                    = deep ( isDecl' >>> hasAttr "id" )

    isDecl'                     = trWithClass (== "decl")

    theTitle                    = ( listA (isDecl' >>> getAttrValue "id") >>. concat )
                                  >>^
                                  unEscapeString

    theSignature                = xshow ( removeSourceLinks
                                          >>>
                                          deep (tdWithClass (== "decl"))
                                          >>>
                                          deep isText
                                        )
                                  >>^
                                  getSignature

    theLinkPrefix               = theSignature
                                  >>^
                                  ( words
                                    >>>
                                    take 1
                                    >>>
                                    concat
                                    >>>
                                    (\ s -> if s `elem` ["data", "type", "newtype"]
                                            then "#t:"
                                            else "#v:"
                                    )
                                  )
    theSourceURI                = ( single
                                    ( ( deep ( ( aWithHref ("src/" `isPrefixOf`)
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

    theModule                   = constA rt >>> getAttrValue "module"
    thePackage                  = constA rt >>> getAttrValue "package"
    theURI                      = constA rt >>> getAttrValue transferURI

-- ------------------------------------------------------------
 
-- | Transform classes so that the methods are wrapped into the same html as normal functions

processClasses                  :: LA XmlTree XmlTree
processClasses                  = processTopDown
                                  ( processClassMethods
                                    `when`
                                    ( getClassPart "section4"
                                      /> hasText (== "Methods")
                                    )
                                  )
    where   
    processClassMethods         = getClassPart "body"
                                  /> hasName "table"
                                  /> hasName "tr"
                                -- getXPathTrees "/tr/td[@class='body']/table/tr/td[@class='body']/table/tr" 

-- ------------------------------------------------------------

-- | Removes Source Links from the XmlTree. A Source Link can be identified by the text of an "a" 
--   node but to be more precise it is also checked whether the href-attribute starts with "src".
--   During the tree transformation it might happen, that source links with empty href attributes 
--   are constructed so empty href attributes are also searched and removed if the text of the "a"
--   node is "Source"

removeSourceLinks               :: LA XmlTree XmlTree
removeSourceLinks               = processTopDown
                                  ( none
                                    `when` 
                                    ( aWithHref (\a -> null a || "src/" `isPrefixOf` a)
                                      />
                                      hasText (== "Source")
                                    )
                                  )      

-- ------------------------------------------------------------

-- | As Haddock can generate Documentation pages with links to source files and without these links
--   there are two different types of declaration table datas. To make the indexing easier, the
--   table datas with source links are transformed to look like those without (they differ 
--   in the css class of the table data and the ones with the source links contain another table).

topdeclToDecl                   :: LA XmlTree XmlTree
topdeclToDecl                   = processTopDownUntil
                                  ( isElemWithAttr "table" "class" (== "declbar")
                                   `guards`
                                   ( getChildren >>> getChildren >>> getChildren )
                                  )
                                  >>>
                                  processTopDownUntil
                                  ( tdWithClass (== "topdecl")
                                    `guards`
                                    mkelem "td" [ sattr "class" "decl"] [ getChildren ]
                                  ) 

-- ------------------------------------------------------------

removeDataDocumentation         :: LA XmlTree XmlTree
removeDataDocumentation         = processTopDown 
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
                                  ( (    tdWithClass   (=="decl")
                                      /> spanWithClass (=="keyword")
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
    getTheName                  = xshow $
                                  deep (hasName "b") /> isText

    getTheRef                   = ( single $
                                    deep (hasName "a" >>> getAttrValue0 "name")
                                  )
                                  `withDefault` ""

    getTheType                  = xshow $
                                  single $
                                  deep (spanWithClass (== "keyword")) /> isText

    getTheSrc                   = ( single $
                                    deep (aWithHref ("src/" `isPrefixOf`))
                                    >>>
                                    getAttrValue0 "href"
                                  )
                                  `withDefault` ""

    mkTheElem n t r s           = eelem "td"
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

processCrazySignatures          :: LA XmlTree XmlTree
processCrazySignatures          = processTopDown
                                  ( preProcessCrazySignature
                                    `when`
                                    getClassPart "rdoc"
                                  )
                                  >>>
                                  processChildren
                                  ( processDocumentRootElement groupDeclSig declAndDocAndSignatureChildren )

preProcessCrazySignature        :: LA XmlTree XmlTree
preProcessCrazySignature        = ( selem "tr" 
                                    [ mkelem "td" [ sattr "class" "signature" ]
                                                  [ deep (tdWithClass (== "arg"))
                                                    >>>
                                                    getChildren
                                                  ]  
                                    ] 
                                    &&&   
                                    selem "tr" 
                                    [ mkelem "td" [ sattr "class" "doc" ]
                                                  [ deep (tdWithClass (== "rdoc"))
                                                    >>>
                                                    getChildren
                                                  ]
                                    ]
                                  )
                                  >>> mergeA (<+>)
         
processDocumentRootElement      :: (LA XmlTree XmlTree -> LA XmlTree XmlTree)
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
           
declAndDocAndSignatureChildren :: LA XmlTree XmlTree
declAndDocAndSignatureChildren = (isDecl <+> isSig <+> isDoc) `guards` this

isDecl                          :: LA XmlTree XmlTree
isDecl                          = trtdWithClass (== "decl")
                                  />
                                  isElemWithAttr "a" "name" (const True)

isDoc                           :: LA XmlTree XmlTree
isDoc                           = trtdWithClass (== "doc")
      
isSig                           :: LA XmlTree XmlTree
isSig                           = trtdWithClass (== "signature")

getDeclName                     :: LA XmlTree String
getDeclName                     = (xshow $ single $ getXPathTrees "//tr/td/a/@name/text()") >>^ drop 2

processTableRows                :: (LA XmlTree XmlTree -> LA XmlTree XmlTree)
                                ->  LA XmlTree XmlTree
                                ->  LA XmlTree XmlTree
processTableRows theGrouping ts = theGrouping (remLeadingDocElems ts) {- >>> prune 3 -}

-- regex for a leading class="doc" row

leadingDoc                      :: XmlRegex
leadingDoc                      = mkStar (mkPrimA isDoc)

-- regex for a class="decl" class="doc" sequence

declSig                         :: XmlRegex
declSig                         = mkSeq (mkPrimA isDecl) (mkSeq (mkStar (mkPrimA isSig)) leadingDoc)

-- remove a leading class="doc" row this does not form a declaration
-- split the list of trees and throw away the first part

remLeadingDocElems              :: LA XmlTree XmlTree -> LA XmlTree XmlTree
remLeadingDocElems ts           = (splitRegexA leadingDoc ts >>^ snd) >>> unlistA

-- group the "tr" trees for a declaration together, build a "tr class="decl"" element and
-- throw the old "tr" s away

groupDeclSig                    :: LA XmlTree XmlTree -> LA XmlTree XmlTree
groupDeclSig ts                 = scanRegexA declSig ts 
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

-- ------------------------------------------------------------

getTitle                        :: LA XmlTree String
getTitle                        = xshow
                                  ( getPath "html/head/title"
                                    >>> deep isText
                                  )

getPackage                      :: LA XmlTree String
getPackage                      = getAttrValue transferURI
                                  >>^   
                                  hayooGetPackage

addPackageAttr                  :: LA XmlTree XmlTree
addPackageAttr                  = this += (attr "package" $ getPackage >>> mkText)

addModuleAttr                   :: LA XmlTree XmlTree
addModuleAttr                   = this += ( attr "module" $ getTitle   >>> mkText)

-- ------------------------------------------------------------

getPath                         :: String -> LA XmlTree XmlTree
getPath                         = foldl (/>) this . map hasName . split "/"

trtdWithClass                   :: (String -> Bool) -> LA XmlTree XmlTree
trtdWithClass av                = hasName "tr"
                                  />
                                  tdWithClass av

getClassPart                    :: String -> LA XmlTree XmlTree
getClassPart c                  = trtdWithClass (== "body")
                                  /> hasName "table"
                                  /> trtdWithClass (== c)

-- ------------------------------------------------------------

-- mother's little helpers

firstChild                      :: LA XmlTree XmlTree -> LA XmlTree XmlTree
firstChild sel                  = single (getChildren >>> sel)


getById                         :: String -> LA XmlTree XmlTree
getById id'                     = single (deep (hasAttrValue "id" (== id')))

firstChildWithClass             :: String -> String -> LA XmlTree XmlTree
firstChildWithClass e c         = firstChild (isElemWithAttr e "class" (== c))

first_p_src                     :: LA XmlTree XmlTree
first_p_src                     = firstChildWithClass "p" "src"
 
divWithClass                    :: (String -> Bool) -> LA XmlTree XmlTree
divWithClass                    = isElemWithAttr "div" "class"

spanWithClass                   :: (String -> Bool) -> LA XmlTree XmlTree
spanWithClass                   = isElemWithAttr "span" "class"

pWithClass                      :: (String -> Bool) -> LA XmlTree XmlTree
pWithClass                      = isElemWithAttr "p" "class"

aWithClass                      :: (String -> Bool) -> LA XmlTree XmlTree
aWithClass                      = isElemWithAttr "a" "class"

trWithClass                     :: (String -> Bool) -> LA XmlTree XmlTree
trWithClass                     = isElemWithAttr "tr" "class"

tdWithClass                     :: (String -> Bool) -> LA XmlTree XmlTree
tdWithClass                     = isElemWithAttr "td" "class"

aWithHref                       :: (String -> Bool) -> LA XmlTree XmlTree
aWithHref                       = isElemWithAttr "a" "href"

-- ------------------------------------------------------------

mkDecl0                         :: String -> LA b XmlTree -> LA b XmlTree
mkDecl0 typ body                = ( eelem "div"
                                    += sattr "class" "top"
                                    += sattr "type"  typ
                                    += body
                                  )

mkDecl1                         :: String -> LA b XmlTree -> LA b XmlTree -> LA b XmlTree
mkDecl1 typ src doc             = mkDecl0 typ
                                  ( ( eelem "p"
                                      += sattr "class" "src"
                                      += src
                                    )
                                    <+>
                                    ( eelem "div"
                                      += sattr "class" "doc"
                                      += doc
                                    )
                                  )

mkFctDecl                       :: LA b XmlTree -> LA b XmlTree -> LA b XmlTree
mkFctDecl                       = mkDecl1 "function"

mkModuleDecl                    :: LA b XmlTree -> LA b XmlTree -> LA b XmlTree
mkModuleDecl                    = mkDecl1 "module"

-- ------------------------------------------------------------

{- tests for splitting function signatures for a list of functions

processFunctionSig          :: LA XmlTree XmlTree
processFunctionSig          = mkSingleFct $< splitMultiFct
    where
      mkSingleFct ts        = replaceChildren (constL ts)
      splitMultiFct         = listA getChildren
                              >>> spanA (neg $ hasText ((== "::") . stringTrim))
                              >>> first ( unlistA
                                          >>> aWithClass (== "def")
                                          >>> hasAttr "name"
                                        ) 
                              >>> arr (uncurry (:))

processFunctionDiv         :: LA XmlTree XmlTree
processFunctionDiv         = (mkSingleDiv $< splitMultiDiv)
                              += sattr "type" "function"
    where
      mkSingleDiv ts        = replaceChildren (constL ts)
      splitMultiDiv         = listA getChildren
                              >>> arr (head &&& tail)
                              >>> first processFunctionSig
                              >>> arr (uncurry (:))

x3 = concat $
     [   "<p class=\"src\">"
     ,     "<a name=\"v:t_xml\" class=\"def\">t_xml</a>"
     ,     ", "
     ,     "<a name=\"v:t_root\" class=\"def\">t_root</a>"
     ,     " :: "
     ,     "<a href=\"/packages/archive/base/4.4.1.0/doc/html/Data-String.html#t:String\">String</a>"
     ,     "<a href=\"src/Text-XML-HXT-DOM-XmlKeywords.html#t_xml\" class=\"link\">Source</a>"
     ,   "</p>"
     ]

x4 = concat $
     [ "<div class=\"top\">"
     , x3
     ,   "<div class=\"doc\">"
     ,     "<p>the predefined namespace uri for xml: &quot;http://www.w3.org/XML/1998/namespace&quot;</p>"
     ,   "</div>"
     , "</div>"
     ]

x2 = "<a><c></c> :: <b></b>::</a>"

show3 = test this x3
show4 = test this x4

-- split a function def like "f1, f2 :: T"
-- in two independent declarations "f1 :: T" and "f2 :: T"
test3 = test processFunctionSig x3

-- split the whole div element containing the function signature
test4  = test processFunctionDiv x4

test f x = sequence $ map putStrLn $ runLA (xread >>> f >>> xshow indentDoc) $ x


-- -}

-- ------------------------------------------------------------
