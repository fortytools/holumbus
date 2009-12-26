-- -----------------------------------------------------------------------------
  -- Uwe Schmidts Regex-Arrow stuff
-- -----------------------------------------------------------------------------

module Hayoo.Regex where

import           Data.Char
import           Data.List

import           Text.XML.HXT.Arrow     
import           Text.XML.HXT.Arrow.XmlRegex

processDocumentRootElement  :: (LA XmlTree XmlTree -> LA XmlTree XmlTree) -> LA XmlTree XmlTree -> LA XmlTree XmlTree
processDocumentRootElement theGrouper interestingChildren
    = processXPathTrees
      (replaceChildren (processTableRows theGrouper (getChildren >>> interestingChildren)))
      "/html/body/table"

processCrazySignatures :: LA XmlTree XmlTree
processCrazySignatures
  =     processTopDown ( preProcessCrazySignature
                         `when`
                         (    hasName "tr"
                           /> hasName "td" >>> hasAttrValue "class" ( (==) "body") 
                           /> hasName "table" 
                           /> hasName "tr" 
                           /> hasName "td" >>> hasAttrValue "class" ( (==) "rdoc") 
                         ) 
                       )
    >>> processChildren (processDocumentRootElement groupDeclSig declAndDocAndSignatureChildren)                      

preProcessCrazySignature :: LA XmlTree XmlTree
preProcessCrazySignature = 
         ( selem "tr" 
                  [ mkelem "td" [sattr "class" "signature"] 
                  [getXPathTrees "//td[@class='arg']" >>> getChildren ]  
                  ] 
           &&&   
           selem "tr" 
                  [ mkelem "td" [sattr "class" "doc"]
                                [getXPathTrees "//td[@class='rdoc']" >>> getChildren ]
                  ]
         ) >>> mergeA (<+>)
         
           
declAndDocChildren  :: LA XmlTree XmlTree
declAndDocChildren  = (isDecl <+> isDoc) `guards` this

declAndDocAndSignatureChildren :: LA XmlTree XmlTree
declAndDocAndSignatureChildren = (isDecl <+> isSig <+> isDoc) `guards` this

isDecl      :: LA XmlTree XmlTree
isDecl
    = hasName "tr" />
      hasName "td" >>> ( hasAttrValue "class" (== "decl")
       `guards`
       ( getChildren >>> hasName "a" >>> hasAttr "name" )
           )

isDoc     :: LA XmlTree XmlTree
isDoc
    = hasName "tr" />
      hasName "td" >>> hasAttrValue "class" (== "doc")
      
isSig    :: LA XmlTree XmlTree
isSig
    = hasName "tr" />
      hasName "td" >>> hasAttrValue "class" (== "signature")

getDeclName     :: LA XmlTree String
getDeclName
    = (xshow $ getXPathTrees "//tr/td/a/@name/text()" >>. take 1) >>^ drop 2
--    = xshow $ (hasName "tr" /> hasName "td" /> hasName "a"  >>> getAttrValue "name") >>^ (drop 1) )
--    >>^ drop 4

processTableRows      :: (LA XmlTree XmlTree -> LA XmlTree XmlTree) -> LA XmlTree XmlTree -> LA XmlTree XmlTree
processTableRows theGrouping ts
    = theGrouping (remLeadingDocElems ts) {- >>> prune 3 -}

-- regex for a leading class="doc" row

leadingDoc    :: XmlRegex
leadingDoc    = mkStar (mkPrimA isDoc)

-- regex for a class="decl" class="doc" sequence

declDoc     :: XmlRegex
declDoc     = mkSeq (mkPrimA isDecl) leadingDoc

declSig     :: XmlRegex
declSig     = mkSeq (mkPrimA isDecl) (mkSeq (mkStar (mkPrimA isSig)) leadingDoc)

    -- remove a leading class="doc" row this does not form a declaration
    -- split the list of trees and throw away the first part
remLeadingDocElems  :: LA XmlTree XmlTree -> LA XmlTree XmlTree
remLeadingDocElems ts   = (splitRegexA leadingDoc ts >>^ snd) >>> unlistA

    -- group the "tr" trees for a declaration together, build a "tr class="decl"" element and
    -- throw the old "tr" s away
groupDeclSig    :: LA XmlTree XmlTree -> LA XmlTree XmlTree
groupDeclSig ts = 
        scanRegexA declSig ts 
    >>> 
    mkelem  "tr" [ sattr "class" "decl"
                 , attr "id" (unlistA >>> getDeclName >>> mkText)
                 ] 
                 [ mkelem "td" [sattr "class" "decl"] 
                          [unlistA >>> getXPathTrees "//td[@class='decl' or @class='signature']" >>> getChildren] 
                 , mkelem "td" [sattr "class" "doc" ]
                          [unlistA >>> getXPathTrees "//td[@class='doc']" >>> getChildren] 
                 ]

{-
groupDeclDoc    :: LA XmlTree XmlTree -> LA XmlTree XmlTree
groupDeclDoc ts   
      = scanRegexA declDoc ts >>>
        mkelem "tr"
        [ sattr "class" "decl"
        , attr  "id" (unlistA >>> getDeclName >>> mkText)
        ] [unlistA >>> getChildren]
-}

isArg :: LA XmlTree XmlTree
isArg = hasName "tr" />  hasName "td" >>> hasAttrValue "class" (== "arg")


removeSpacers :: LA XmlTree XmlTree
removeSpacers =
  processTopDown
          (  none
            `when`
             (hasName "tr" /> hasName "td" >>> hasAttrValue "class" (\a ->  ( a == "s15" || a == "s8" ) ) )
          )

