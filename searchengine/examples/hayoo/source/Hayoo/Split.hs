-- -----------------------------------------------------------------------------    
--   Creation of Virtual Documents
-- -----------------------------------------------------------------------------    

module Hayoo.Split where

import           Hayoo.Common
import           Hayoo.Config
import           Hayoo.Regex

-- import           Control.Monad hiding (join, when)

-- import           Data.Binary
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.UTF8 (ByteString)
-- import           Data.Char
import           Data.List
import           Data.Maybe


import           Holumbus.Build.Config
import           Holumbus.Build.Index

import           Holumbus.Index.Common
import           Holumbus.Index.Documents
import           Holumbus.Utility

import           Network.URI(unEscapeString)

import           Text.XML.HXT.Arrow
import           Text.XML.HXT.XPath     


-- | Function to split a document into virtual documents where virtual document contains the
--   declaration of a function (or data, newtype, ...) and optionally the documentation of this
--   element. This runs as the MAP part of a MapReduce Computation
getVirtualDocs :: String -> Int -> URI -> IO [(Int, (String, String, FunctionInfo))]
getVirtualDocs splitPath docId theUri =         
  runX ( 
      traceMsg 1 ("splitting document: " ++ theUri)
  >>> readDocument standardReadDocumentAttributes theUri        -- 1. read the Document
  >>> fromLA (     processClasses                               -- 2. transform the html for classes
                                                                --    to function-like html
               >>> topdeclToDecl                                -- 3. transform declarations with 
                                                                --    source-links to ones without
               >>> removeDataDocumentation                      -- 4. remove documentation for datas
               >>> processDataTypeAndNewtypeDeclarations        -- 5. transform declarations to  
                                                                --    function-like html
               >>> processCrazySignatures                       -- 6. transform multi-line 
              )                                                 --    declarations
                                                                -- 7. split the document
  >>> makeVirtualDocs $<< (     (getXPathTrees "/html/head/title/text()" >>> getText)
                            &&& (getAttrValue "transfer-URI" >>^ packageFromURI)
                          )
  >>> (constA 42 &&& this) -- TODO maybe it is not the answer to THIS particular question
  )    
  where
  makeVirtualDocs theModule thePackage =
           getXPathTrees "//tr[@class='decl' and @id]"
    >>> (  mkVirtual $<<<< (     (     getXPathTreesInDoc "/tr[@class='decl']/@id/text()"
                                    >>> getText
                                    >>^ unEscapeString
                                  )
                              &&& constA (theModule, thePackage)
                              &&& fromLA getTheSignature
                              &&& getSourceLink
                            )      
         )
  mkVirtual theTitle (theModule, thePackage) theSignature theSourceURI =     
         let theLinkPrefix = if theSignature `elem` ["data", "type", "newtype"] then "#t:" else "#v:" 
         in
         root [] [ 
                   selem "table" [ this
                                 , selem "tr" [ mkelem "td" [sattr "id" "module"] 
                                                            [constA theModule >>> mkText]
                                              , mkelem "td" [sattr "id" "package"]
                                                            [constA thePackage >>> mkText]
                                              ]
                                 ]
                 ]
                 >>> writeDocument [("a_output_xml", "1"), ("a_indent", "1")] ((splitPath ++ tmpFile docId theUri) ++ (escape (theLinkPrefix ++ theTitle)))
     >>> (     constA theTitle 
           &&& constA (theUri ++ theLinkPrefix ++ theTitle)
           &&& constA (FunctionInfo (pack theModule) (pack theSignature) (pack thePackage) (theSourceURI))
         )
     >>^ (\(a,(b,c)) -> (a,b,c)) 
  getTheSignature =     removeSourceLinks 
                    >>> fromLA ( listA ( xshow ( (cc_fExtract ccHayooSignature) >>> getTexts))) --TODO 
                    >>^ concat >>^ getSignature
  getSourceLink :: ArrowXml a => a XmlTree (Maybe ByteString)
  getSourceLink = 
      (      processTopDown ( getChildren `when` (isElem >>> hasName "a" >>> hasAttr "name")  )
        >>> getXPathTreesInDoc "//td[@class='decl']/a/@href/text()"  >>> getText
        >>^ (\a -> if "src/" `isPrefixOf` a then expandURIString a theUri else Nothing)
        >>^ (\a -> if isNothing a then Nothing else Just (pack ( fromJust a)) )
      ) `withDefault` Nothing

-- | Transform classes so that the methods are wrapped into the same html as normal functions
processClasses :: LA XmlTree XmlTree
processClasses = 
  processTopDown (  processClassMethods
                   `when`
                   (    hasName "tr"
                     /> hasName "td" >>> hasAttrValue "class" (== "body")
                     /> hasName "table"
                     /> hasName "tr"
                     /> hasName "td" >>> hasAttrValue "class" (== "section4")
                     /> hasText (== "Methods")
                   )
                 )
  where               
    processClassMethods :: LA XmlTree XmlTree
    processClassMethods = getXPathTrees "//td[@class='body']/table/tr/td[@class='body']/table/tr" 

-- | Removes Source Links from the XmlTree. A Source Link can be identified by the text of an "a" 
--   node but to be more precise it is also checked whether the href-attribute starts with "src".
--   During the tree transformation it might happen, that source links with empty href attributes 
--   are constructed so empty href attributes are also searched and removed if the text of the "a"
--   node is "Source"
removeSourceLinks :: LA XmlTree XmlTree
removeSourceLinks = 
  processTopDown ( none `when` 
                   (     hasName "a" 
                     >>> hasAttrValue "href" (\a -> "src/" `isPrefixOf` a || length a == 0) 
                     />  hasText (== "Source") )
                 )      


-- | As Haddock can generate Documentation pages with links to source files and without these links
--   there are two different types of declaration table datas. To make the indexing easier, the
--   table datas with source links are transformed to look like those without (they differ 
--   in the css class of the table data and the ones with the source links contain another table).
topdeclToDecl ::LA XmlTree XmlTree
topdeclToDecl  
    =     processTopDown
            ( (getChildren >>> getChildren >>> getChildren)
              `when`
              (isElem >>> hasName "table" >>> hasAttrValue "class" (== "declbar"))
            )
      >>> processTopDown
            ( mkelem "td" [ sattr "class" "decl"] [ getChildren ]
              `when`
              ( isElem >>> hasName "td" >>> hasAttrValue "class" (== "topdecl") )
            ) 

-- | The REDUCE phase of the virtual d>>^ drop 1 >>^ headocument creation MapReduce computation.
--   The virtual Documents from the MAP phase are collected and a new Documents data is created
mkVirtualDocList :: Int -> [(String, String, FunctionInfo)] -> IO (Maybe (Documents  FunctionInfo))
mkVirtualDocList _ vDocs
  = return $! Just $ foldl' (\d r -> snd (insertDoc d r)) 
                             emptyDocuments 
                            (map (\(t, u, fi) -> Document t u (Just fi)) vDocs)
            

-- -----------------------------------------------------------------------------
{-
processDataTypeAndNewtypeDeclarations :: LA XmlTree XmlTree
processDataTypeAndNewtypeDeclarations 
  =  processTopDown
          ( ( ( mkTheElem $<<<< ( getTheName
                              &&& getTheType
                              &&& getTheRef
                              &&& getTheSrc
                            ) 
          ) 
          `when`
          (   hasName "table" >>> hasAttrValue "class" (== "declbar")   -- declbar table -> table for declarations
           /> hasName "tbody"
           /> hasName "tr"
           /> hasName "td"    >>> hasAttrValue "class" (=="declname")   -- "declname", not "decl"
           /> hasName "span"  >>> hasAttrValue "class" (=="keyword")    -- for data, type or class
           /> hasText (`elem` ["data", "type", "class"]) 
          )
        )
      ) 
  where
  getTheSrc
      = ( getXPathTrees "//td[@class='declbut']/a[@href]"               -- source link
          >>>
          ( ( getChildren >>> hasText (== "Source") )
            `guards`
            getAttrValue "href"
          )
        ) `withDefault` ""                                              -- make it deterministic

  getTheName = xshow $ getXPathTrees "//td[@class='declname']/b/text()"                        -- xshow is deterministic
  getTheRef  = xshow $ getXPathTrees "//td[@class='declname']/a/@name/text()" >>. take 1        -- the 1. <a name="..."> is the interesting
  getTheType = xshow $ getXPathTrees "//td[@class='declname']/span[@class='keyword']/text()"    -- data, type or newtype

  mkTheElem n t r s
      = eelem "table"                                                   -- another way of building xml trees
        += sattr "class" "declbar"                                      -- make an empty element and add attributes and contents
        += sattr "title" "data, type or newtype"                        -- just a marker for transformed xml trees
        += ( eelem "tbody"
             += ( eelem "tr"
                  += ( eelem "td"
                       += sattr "class" "decl"
                       += ( eelem "a"
                            += sattr "name" r )
                       += txt (n ++ " :: " ++ t)
                       += ( eelem "a"
                            += sattr "href" s
                            += txt "" )
                     )
                )
           )

-}

processDataTypeAndNewtypeDeclarations :: LA XmlTree XmlTree
processDataTypeAndNewtypeDeclarations 
  = processTopDown
      ( ( mkTheElem $<<<< (      getTheName
                             &&& getTheType
                             &&& getTheRef
                             &&& getTheSrc
                           ) 
         ) 
         `when`
         (   
               hasName "td"   >>> hasAttrValue "class" (=="decl")
            /> hasName "span" >>> hasAttrValue "class" (=="keyword")
            /> hasText (\a -> a `elem` ["data", "type", "newtype", "class"]) 
         )
      ) 
    where
      getTheSrc = ( getXPathTrees "//td[@class='declbut']/a[@href]"               -- source link
                    >>>
                    ( ( getChildren >>> hasText (== "Source") )
                      `guards`
                      getAttrValue "href"
                    )
                  ) `withDefault` ""  
--                listA ( 
--                       getXPathTrees "//a[@href]/@href/text()"                 >>> getText
--                   >>> arr (\a -> if "src/" `isPrefixOf` a then Just a else Nothing) ) >>^ catMaybes 
--                   >>^ (\a -> if length a == 0 then "" else last a)  -- TODO this could be done more beautiful
      getTheName = xshow $ getXPathTrees "//b/text()"                            
      getTheRef  = xshow $ getXPathTrees "//a/@name/text()" >>. take 1 -- >>> constA "fooblubb" -- >>> traceMsg 0 "foo" >>> this -- >>> arr (\s -> traceMsg 0 s) -- >>^ drop 1 >>^ take 1
      getTheType = xshow $ getXPathTrees "//span[@class='keyword']/text ()" >>. take 1
      mkTheElem n t r s = 
            mkelem "td" [sattr "class" "decl"]
            [ aelem "a" [sattr "name" r] 
                        , constA (n ++ " :: " ++ t) >>> mkText
                        , mkelem "a" [sattr "href" s] [constA " " >>> mkText] 
                        ] 


removeDataDocumentation :: LA XmlTree XmlTree
removeDataDocumentation 
  = processTopDown 
      (
        none
        `when`
        (    hasName "tr"
          /> hasName "td" >>> hasAttrValue "class" (== "body")
          /> hasName "table"
          /> hasName "tr"
          /> hasName "td" >>> hasAttrValue "class" (== "section4")
          /> hasText (\a -> a == "Constructors" || "Instances" `isSuffixOf` a)
        )
      )
      
packageFromURI :: String -> String
packageFromURI u = if "http://www.haskell.org/gtk2hs/docs/current/" `isPrefixOf` u
                     then "gtk2hs"
                     else if "http://hackage.haskell.org/packages/archive/" `isPrefixOf` u
                            then (split "/" u) !! 5
                            else "unknown package"
