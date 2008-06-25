-- ----------------------------------------------------------------------------

{- |
  Module     : HayooIndexer
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: untested
  Version    : 0.1

  Indexer for the Haskell API Documentation Search Engine Hayoo!

  TODO: 
  - Include class, data, ... descriptions.

-}

-- ----------------------------------------------------------------------------
{-# OPTIONS -farrows -fglasgow-exts #-}
-- ----------------------------------------------------------------------------

module Main where

-- import           HayooConfig


import           Data.Binary
import           Control.Monad hiding (join,when)

import           Data.Char
import           qualified Data.IntMap as IM
import           qualified Data.Map    as M
import           Data.List
import           Data.Maybe

import           Holumbus.Build.Crawl
import           Holumbus.Build.Index
import           Holumbus.Build.Config
import           Holumbus.Control.MapReduce.ParallelWithClass
-- import           Holumbus.Index.Cache

import           Holumbus.Index.Common
import           Holumbus.Index.Inverted.Memory(emptyInverted)
import           Holumbus.Index.Documents
import qualified Holumbus.Index.Documents as DOC  
import           Holumbus.Utility

import           Network.URI(unEscapeString)

import qualified Data.Set as S

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML
import           Text.XML.HXT.Arrow.XmlRegex

main :: IO ()
main 
  = do
    -- ---------------------------------------------------------------------------------------------
       -- Configuration
    let traceLevel    = 1
        workerThreads = 5 
        docsPerCrawl  = 100
        docsPerIndex  = 500
        splitPath     = "/tmp/"
        indexPath     = "/home/sms/hayoo"
        idxConfigs    = []
                              ++ [ic_Hayoo]            -- hackage.haskell.org
--                              ++ [ic_GHC_libs]         -- all GHC libs
--                              ++ [ic_HXT]              -- hxt arrow & filter docs
                              ++ [ic_Holumbus]         -- holumbus framework
--                              ++ [ic_Single]            -- one document for debugging
        idxConfig     = foldl1 mergeIndexerConfigs idxConfigs

        crawlerState  = (initialCrawlerState idxConfig customCrawlFunc)-- {cs_fPreFilter = preCrawlFilter}


    
{-    crawlerState <- loadCrawlerState "/tmp/CS.bin" crawlerState
    crawlerState <- return crawlerState 
      {cs_toBeProcessed = S.filter hayooFilter 
                                   -- (\s -> not ("Graphics-UI-WXCore-WxcClassesAL.html" `isSuffixOf` s)) 
                                   (cs_toBeProcessed crawlerState)}
-}
                          
    -- ---------------------------------------------------------------------------------------------
       -- find available documents and save local copies as configured 
       -- in the IndexerConfig
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl traceLevel workerThreads docsPerCrawl crawlerState
    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-predocs.xml") docs
    
    

--    docs <- loadFromXmlFile ( (ic_idxPath idxConfig) ++ "-predocs.xml") :: IO (Documents FunctionInfo)
    
    docs <- return $ filterDocuments (\d -> isPrefixOf "http://hackage.haskell.org/packages/archive/" (uri d)) docs
    docs <- return $ filterDocuments (\d -> not (isSuffixOf "pkg-list.html" (uri d))) docs
    docs <- return $ filterDocuments (\d -> not (isSuffixOf "recent.html" (uri d))) docs
    docs <- return $ filterDocuments (hayooFilter . uri) docs
    docs <- return $ filterDocuments (isLatestVersion (getVersions docs)) docs
    
    writeToXmlFile "/tmp/docs.xml" docs

    
    
    
    -- ---------------------------------------------------------------------------------------------
       -- split the locally saved documents into several small xml files
       -- where each file consists of the documentation for one function
    runX (traceMsg 0 (" splitting ----------------------------- " ))
    splitDocs' <- mapReduce 
                    workerThreads 
                    (getVirtualDocs traceLevel splitPath)
                    mkVirtualDocList
                    (IM.toList (IM.map uri (DOC.toMap docs)))
    splitDocs  <-  return $! snd (M.elemAt 0 splitDocs')
    writeToXmlFile ( indexPath ++ "-docs.xml") (splitDocs)
    writeToBinFile ( indexPath ++ "-docs.bin") (splitDocs)

    -- ---------------------------------------------------------------------------------------------
       -- build an index over the split xml files
    runX (traceMsg 0 (" indexing  ------------------------------ " ))
    localDocs <- return $ tmpDocs splitPath splitDocs
    
    -- cache     <- createCache ((ic_idxPath idxConfig) ++ "-cache.db")
    
    pathes    <- buildSplitIndex workerThreads traceLevel localDocs idxConfig
                              emptyInverted True docsPerIndex 
    idx       <- foldM mergeIndexes' emptyInverted pathes
    
    writeToXmlFile ( indexPath ++ "-index.xml") idx
    writeToBinFile ( indexPath ++ "-index.bin") idx
    
    return ()
    where 
      mergeIndexes' i1 f = do
                           i2 <- loadFromBinFile (f  ++ "-index.bin")
                           return $ mergeIndexes i1 i2



isLatestVersion :: M.Map String String -> Document a -> Bool
isLatestVersion m d = ver == M.findWithDefault "0" pkg m
  where 
  pkg = (split "/" (uri d)) !! 5
  ver = (split "/" (uri d)) !! 6

getVersions :: Binary a => Documents a -> M.Map String String
getVersions docs = foldl' (\m d -> M.insertWith cmp (pkg d) (ver d) m) M.empty
                          (map snd $ IM.toList $ DOC.toMap docs)
  where 
  cmp v1 v2 = if v1 >= v2 then v1 else v2
  pkg d     = (split "/" (uri d)) !! 5
  ver d     = (split "/" (uri d)) !! 6
          

hayooFilter :: URI -> Bool
hayooFilter = simpleCrawlFilter [ "http://hackage.haskell.org/" ]                  -- allow
                                         [ "/src/", "/trac/", ".tar.gz$", ".cabal$", ".pdf$"-- deny
                                         , "/doc-index-", "doc-index.html", "/logs/failure/" 
                                         , "http://hackage.haskell.org/cgi-bin/hackage-scripts/list-users"
                                         , "http://hackage.haskell.org/packages/hackage.html"
                                         , "Data-TypeLevel-Num-Aliases.html"
                                         , "Graphics-X11-Types.html"
                                         , "Harpy-X86Assembler.html"
                                         , "Database-HaskellDB-BoundedList.html"
                                         , "Graphics-UI-WXCore-WxcClassTypes.html"
                                         , "Graphics-UI-WXCore-WxcClassesAL.html"
                                         , "Graphics-UI-WXCore-WxcClassesMZ.html"
                                         , "Graphics-UI-WXCore-WxcDefs.html"
                                         ]


-- -------------------------------------------------------------------------------------------------

-- | dummy function for the custom information in the Documents. This is used in the crawl phase
--   of the index generation. The real custom information is computed during the split phase where
--   every document is split up into pseude-documents which contain exactly one function
customCrawlFunc :: IOSArrow XmlTree (Maybe FunctionInfo)
customCrawlFunc = constA Nothing

    
-- -----------------------------------------------------------------------------    
   -- Dirty Stuff for the Creation of Virtual Documents
-- -----------------------------------------------------------------------------    

-- | Function to split a document into virtual documents where virtual document contains the
--   declaration of a function (or data, newtype, ...) and optionally the documentation of this
--   element. This runs as the MAP part of a MapReduce Computation
getVirtualDocs :: Int -> String -> Int -> URI -> IO [(Int, (String, String, FunctionInfo))]
getVirtualDocs traceLevel splitPath docId theUri =         
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
 -- >>> writeDocument [(a_indent, "1")] "/home/sms/tmp/crazy.xml"
                                                                -- 7. split the document
  >>> makeVirtualDocs $< (getXPathTrees "/html/head/title/text()" >>> getText)
  >>> (constA 42 &&& this) -- TODO maybe it is not the answer to THIS particular question
  )    
  where
  makeVirtualDocs theModule =
           getXPathTrees "//tr[@class='decl' and @id]"
    >>> (  mkVirtual $<<<< (     (     getXPathTreesInDoc "/tr[@class='decl']/@id/text()"
                                   >>> getText
                                   >>^ unEscapeString
                                 )
                             &&& constA theModule
                             &&& fromLA getTheSignature
                             &&& getSourceLink
                           )      
         )
  mkVirtual theTitle theModule theSignature theSourceURI =     
         let theLinkPrefix = if theSignature `elem` ["data", "type", "newtype"] then "#t:" else "#v:" 
         in
         root [] [ this
                 , selem "module" [constA theModule >>> mkText]
                 ]
     >>> writeDocument [("a_output_xml", "1")] ((splitPath ++ tmpFile docId theUri) ++ (escape (theLinkPrefix ++ theTitle)))
     >>> (     constA theTitle 
           &&& constA (theUri ++ theLinkPrefix ++ theTitle)
           &&& constA (FunctionInfo theModule theSignature (theSourceURI))
         )
     >>^ (\(a,(b,c)) -> (a,b,c)) 
  getTheSignature =     removeSourceLinks >>> preFilterSignatures >>> getXPathTreesInDoc theHayooXPath 
                 >>> getText
                 >>^ dropWhile ( /= ':' ) >>^ drop 3 
  getSourceLink :: ArrowXml a => a XmlTree (Maybe String)
  getSourceLink = 
      (      processTopDown ( getChildren `when` (isElem >>> hasName "a" >>> hasAttr "name")  )
        >>> getXPathTreesInDoc "//td[@class='decl']/a/@href/text()"  >>> getText
        >>^ (\a -> if "src/" `isPrefixOf` a then expandURIString a theUri else Nothing)
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


-- | As Haddock can generate Documetation pages with links to source files and without these links
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

-- | The REDUCE phase of the virtual document creation MapReduce computation.
--   The virtual Documents from the MAP phase are collected and a new Documents data is created
mkVirtualDocList :: Int -> [(String, String, FunctionInfo)] -> IO (Maybe (DOC.Documents  FunctionInfo))
mkVirtualDocList _ vDocs
  = return $! Just $ foldl' (\d r -> snd (insertDoc d r)) 
                             DOC.emptyDocuments 
                            (map (\(t, u, fi) -> Document t u (Just fi)) vDocs)
            

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------

{-
removeElementsByXPath :: ArrowXml a => String -> a XmlTree XmlTree 
removeElementsByXPath xpath =
  case xpath of
       "//td[@class='rdoc']"  ->  processTopDown (none `when` (isElem >>> hasName "td" >>> hasAttrValue "class" (== "rdoc")))
       otherwise              ->  none
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
      getTheSrc  =  listA ( 
                       getXPathTrees "//a[@href]/@href/text()"                 >>> getText
                   >>> arr (\a -> if "src/" `isPrefixOf` a then Just a else Nothing) ) >>^ catMaybes 
                   >>^ (\a -> if length a == 0 then "" else last a)  -- TODO this could be done more beautiful
      getTheRef  = getXPathTrees "//a[@name]/@name/child::text()"          >>> getText
      getTheName = getXPathTrees "//b/text()"                              >>> getText
      getTheType = listA (getXPathTrees "//span[@class='keyword']/text ()" >>> getText) >>^ head
      mkTheElem n t r s = 
            mkelem "td" [sattr "class" "decl"]
                        [ aelem "a" [sattr "name" r] 
                        , constA (n ++ " :: " ++ t) >>> mkText
                        , mkelem "a" [sattr "href" s] [constA "Source" >>> mkText]
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
--          /> hasName "tbody"
          /> hasName "tr"
          /> hasName "td" >>> hasAttrValue "class" (== "section4")
          /> hasText (\a -> a == "Constructors" || "Instances" `isSuffixOf` a)
        )
      )


-- -----------------------------------------------------------------------------
   -- Uwe Schmidts Regex-Arrow stuff
-- -----------------------------------------------------------------------------
-- tt h (topdeclToDecl >>> removeElementsByXPath "//td[@class='rdoc']" >>> processTopDown (none `when` (hasName "td" >>> hasAttrValue "class" ( (==) "body") /> hasName "table" /> hasName "tr" /> hasName "td" >>> hasAttrValue "class" ( (==) "ndoc") ) ) ) 

-- processOldCrazy :: LA XmlTree XmlTree
-- processOldCrazy = processChildren (processDocumentRootElement groupDeclDoc declAndDocChildren)

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
                           /> hasName "td" >>> hasAttrValue "class" ( (==) "ndoc") 
                         ) 
                       )
    >>> processChildren (processDocumentRootElement groupDeclSig declAndDocAndSignatureChildren)                      

preProcessCrazySignature :: LA XmlTree XmlTree
preProcessCrazySignature = 
         ( selem "tr" 
                  [ mkelem "td" [sattr "class" "signature"] 
                                [getXPathTrees "//td[@class='arg']" >>> getChildren  ]  
                  ] 
           &&&   
           selem "tr" 
                  [ mkelem "td" [sattr "class" "doc"]
                                [getXPathTrees "//td[@class='ndoc']" >>> getChildren ]
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
    = listA (hasName "tr" /> hasName "td" /> hasName "a" >>> getAttrValue "name")>>. concat
      >>> (arr $ drop 4)

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

groupDeclDoc    :: LA XmlTree XmlTree -> LA XmlTree XmlTree
groupDeclDoc ts   = scanRegexA declDoc ts >>>
--        mkelem "function"
--        [ attr  "name" (unlistA >>> getDeclName >>> mkText)
        mkelem "tr"
        [ sattr "class" "decl"
        , attr  "id" (unlistA >>> getDeclName >>> mkText)
        ] [unlistA >>> getChildren]

isArg     :: LA XmlTree XmlTree
isArg
    = hasName "tr" />
      hasName "td" >>> hasAttrValue "class" (== "arg")


removeSpacers :: LA XmlTree XmlTree
removeSpacers =
  processTopDown
          (  none
            `when`
             (hasName "tr" /> hasName "td" >>> hasAttrValue "class" (\a ->  ( a == "s15" || a == "s8" ) ) )
          )








-- -------------------------------------------------------------------------------------------------
--  Debugging stuff, will be removed soon
-- -------------------------------------------------------------------------------------------------
          
{-
p ps xpathExpr = processFromNodeSet ps $< getXPathNodeSet xpathExpr
wd = writeDocument [(a_indent, "1")] "/home/sms/tmp/crazy.xml"
wt = writeDocument [(a_show_tree, "1")] "/home/sms/tmp/crazy.xml"
t f a = runX (readDocument stdOpts4Reading f >>> a)
tt f a 
  = runX (readDocument stdOpts4Reading f >>> a >>> fromLA removeSpacers >>> writeDocument [] "" >>> constA "")
h = "http://holumbus.org/docs/develop/Holumbus-Index-Common.html"
f ="/home/sms/tmp/holumbus_docs/http%3a%2f%2fwww%2eholumbus%2eorg%2fdocs%2fdevelop%2fHolumbus%2dControl%2dMapReduce%2dParallel%2ehtml"
fs="/home/sms/tmp/holumbus_docs/split/http%3a%2f%2fwww%2eholumbus%2eorg%2fdocs%2fdevelop%2fHolumbus%2dUtility%2ehtml%23v%3ajoin"
c = "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Exception.html"

cl = "http://www.holumbus.org/docs/develop/Holumbus-Index-Common.html"
da = "http://www.holumbus.org/docs/develop/Holumbus-Build-Index.html"

c1 = "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc_arrow/Control-Arrow-ArrowList.html"
c2 = "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc_arrow/Text-XML-HXT-Arrow-XmlArrow.html"
-}



      
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
-- -----------------------------------------------------------------------------    
   -- Crawler & Indexer Configuration
-- -----------------------------------------------------------------------------    
{- ic_Single :: IndexerConfig
ic_Single
 = mkIndexerConfig
--    [ "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc_arrow/Control-Arrow-ArrowList.html"
--    , "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc_arrow/Text-XML-HXT-Arrow-XmlArrow.html"
--    , "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Exception.html"
--    [ "http://www.holumbus.org/docs/develop/Holumbus-Build-Index.html"
--    , "http://www.holumbus.org/docs/develop/Holumbus-Index-Common.html"
--    ,
    [ "http://www.haskell.org/ghc/docs/latest/html/libraries/Win32/System-Win32-SimpleMAPI.html"
    ]
    (Just "/home/sms/tmp/")
    "/home/sms/indexes/exception"
    ccs_Hayoo
    stdOpts4Reading
    []
    [".*"]
-}

preCrawlFilter :: ArrowXml a => a XmlTree XmlTree -- LA XmlTree XmlTree
preCrawlFilter 
  = processTopDown (
      none
      `when`
      (    hasName "tr" 
        /> hasName "th" >>> hasAttrValue "class" (== "horizontal")
        /> hasText (\a -> a `elem` ["Other versions", "Dependencies"])
      )
    )

ic_Hayoo :: IndexerConfig
ic_Hayoo
  = IndexerConfig
    { ic_startPages      = [ "http://hackage.haskell.org/packages/archive/pkg-list.html" ]
    , ic_tmpPath         = Just "/tmp/"
    , ic_idxPath         = "/home/sms/hayoo"
    , ic_contextConfigs  = ccs_Hayoo
    , ic_readAttributes  = standardReadDocumentAttributes
    , ic_fCrawlFilter    = hayooFilter
    }

ic_Holumbus :: IndexerConfig
ic_Holumbus 
  =  IndexerConfig
     { ic_startPages     = [ "http://holumbus.fh-wedel.de/docs/develop/index.html" ]
     , ic_tmpPath        = Just "/home/sms/tmp/holumbus_docs/"
     , ic_idxPath        = "/home/sms/indexes/holumbus"
     , ic_contextConfigs = ccs_Hayoo                                   
     , ic_readAttributes = standardReadDocumentAttributes
     , ic_fCrawlFilter   = simpleCrawlFilter [ "http://holumbus.fh-wedel.de/docs/develop/" ] [ "/src/"]
     }      

ic_GHC_libs :: IndexerConfig         
ic_GHC_libs
 = IndexerConfig 
   { ic_startPages      = [ "http://www.haskell.org/ghc/docs/latest/html/libraries/"
                          , "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Exception.html"
                          ]
   , ic_tmpPath         = Just "/home/sms/tmp/ghc_libs/"
   , ic_idxPath         = "/home/sms/indexes/ghclibs"
   , ic_contextConfigs  = ccs_Hayoo
   , ic_readAttributes  = standardReadDocumentAttributes
   , ic_fCrawlFilter    = simpleCrawlFilter [ "^http://www.haskell.org/ghc/docs/latest/html/" ]
                                        [ "/src/" 
                                        , "^http://www.haskell.org/ghc/docs/latest/html/libraries/doc-index.html" 
                                        , "/logs/failure/"
                                        , "tar.gz$"
                                        ]   
   }

ic_HXT :: IndexerConfig
ic_HXT 
  = IndexerConfig
    { ic_startPages     = [ "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/index.html"]
    , ic_tmpPath        = Just "/tmp/"
    , ic_idxPath        = "/home/sms/indexes/hxt"
    , ic_contextConfigs = ccs_Hayoo
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "^http://www.fh-wedel.de/~si/HXmlToolbox/hdoc"]
                                        [ "/src/"]
    }
  
ccs_Hayoo :: [ContextConfig]
ccs_Hayoo = [ ccModule
            , ccHierarchy
            , ccHayooName     
            , ccHayooPartialName 
            , ccHayooSignature 
            , ccHayooNormalizedSignature
            , ccHayooDescription
            ]
            
theHayooXPath :: String
-- theHayooXPath =  "/function//text()" 
theHayooXPath =  "/tr/td[@class='decl']/text()" 
            
ccHayooName :: ContextConfig
ccHayooName = ContextConfig "name" (fromLA preFilterSignatures) theHayooXPath  
              (\a -> [stripWith (\b -> b `elem` "():") (strip (takeWhile ( (/=) ' ') (dropWhile ( (==) ' ') a)))])
              (\s -> (s == "type" || s == "class" || s == "data"))  False

ccHayooPartialName :: ContextConfig
ccHayooPartialName = ContextConfig "partial" (fromLA preFilterSignatures) theHayooXPath 
                     partTokenize (\s -> (s == "type" || s == "class" || s == "data")) False 
  where 
    partTokenize s = split " " (deCamel False (takeWhile ( (/=) ' ') s))
    deCamel :: Bool -> String -> String  -- Bool flag: last character was a capital letter
    deCamel _ []     = []
    deCamel wasCap (x:xs) = if x == '_' 
                             then ' ' : deCamel True xs
                             else 
                               if x `elem` ['A'..'Z']
                                 then 
                                   if wasCap
                                     then x: deCamel True xs
                                     else ' ' : (x : deCamel True xs) 
                                 else x : deCamel (x `elem` ['A'..'Z']) xs

ccHayooSignature :: ContextConfig
ccHayooSignature = ContextConfig "signature" (fromLA preFilterSignatures) theHayooXPath (\a -> [getSignature a]) (\s -> length s == 0) False

getSignature :: String -> String
getSignature s = if "=>" `isInfixOf` s
                      then stripSignature (drop 3 (dropWhile ((/=) '=') s)) -- TODO split!
                      else stripSignature (drop 3 (dropWhile ((/=) ':') s))


ccHayooNormalizedSignature :: ContextConfig
ccHayooNormalizedSignature = ContextConfig "normalized" (fromLA preFilterSignatures) theHayooXPath 
  (\s -> [normalizeSignature (getSignature s)]) (\s -> length s ==  0) False

-- | Configruation for description context.
ccHayooDescription :: ContextConfig 
ccHayooDescription 
  = ContextConfig { cc_name        = "description" 
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_XPath       = "//td[@class='doc']//text()"
                  , cc_fTokenize   = map (stripWith (=='.')) . (parseWords isWordChar)
                  , cc_fIsStopWord = (\s -> length s < 2)
                  , cc_addToCache  = True
                  }

ccHaddockSignatures :: ContextConfig
ccHaddockSignatures  
  = ContextConfig { cc_name        = "signatures"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_XPath       = theHayooXPath
                  , cc_fTokenize   = (\a -> [a])
                  , cc_fIsStopWord = const False
                  , cc_addToCache  = False     
                  }
                  
ccModule :: ContextConfig
ccModule 
  = ContextConfig { cc_name        = "module"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_XPath       = "/module/text()" 
                  , cc_fTokenize   = \a -> [a]
                  , cc_fIsStopWord = const False     
                  , cc_addToCache  = False
                  }

ccHierarchy :: ContextConfig
ccHierarchy = ContextConfig "hierarchy" (fromLA preFilterSignatures) "/module/text()" (split ".") (const False) False     
            
preFilterSignatures :: LA XmlTree XmlTree
preFilterSignatures
    =     flattenElementsByType "b"
      >>> flattenElementsByType "a"
      >>> flattenElementsByType "span"
--      >>> flattenElementsByType "tt"
      >>> flattenElementsByType "em"
      >>> flattenElementsByType "p"
      >>> flattenElementsByType "pre"
      >>> removeDeclbut  -- redundant !?

flattenElementsByType :: String -> LA XmlTree XmlTree
flattenElementsByType s
    = processTopDown ( getChildren
                       `when`
                       hasName s
                     )

removeDeclbut :: ArrowXml a => a XmlTree XmlTree
removeDeclbut =
       processTopDown 
          (  none
            `when`
             isDeclbut
          )
          
isDeclbut :: ArrowXml a => a XmlTree String
isDeclbut
            = isElem 
          >>> hasName "td" 
          >>> hasAttr "class" 
          >>> getAttrValue "class" 
          >>> isA declbutAttr
          where
          declbutAttr = isPrefixOf "declbut" 
  
  
-- -------------------------------------------------------------------------------------------------
-- stuff from Hayoo.Common

-- | Additional information about a function.
data FunctionInfo = FunctionInfo 
  { moduleName :: String      -- ^ The name of the module containing the function, e.g. Data.Map
  , signature :: String       -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
  , sourceURI :: Maybe String -- ^ An optional URI to the online source of the function.
  } 
  deriving (Show, Eq)

instance XmlPickler FunctionInfo where
  xpickle = xpWrap (\(m, s, r) -> FunctionInfo m s r, \(FunctionInfo m s r) -> (m, s, r)) xpFunction
    where
    xpFunction = xpTriple xpModule xpSignature xpSource
      where -- We are inside a doc-element, therefore everything is stored as attribute.
      xpModule = xpAttr "module" xpText0
      xpSignature = xpAttr "signature" xpText0
      xpSource = xpOption (xpAttr "source" xpText0)

instance Binary FunctionInfo where
  put (FunctionInfo m s r) = put m >> put s >> put r
  get = liftM3 FunctionInfo get get get
  
-- | Normalizes a Haskell signature, e.g. @String -> Int -> Int@ will be transformed to 
-- @a->b->b@. All whitespace will be removed from the resulting string.
normalizeSignature :: String -> String
normalizeSignature = join "->" . (replaceTypes M.empty ['a'..'z']) . split "->" . filter (not . isSpace)
  where
  replaceTypes _ _ [] = []
  replaceTypes v t (x:xs) = let (nv, ut, rx) = replace in rx:(replaceTypes nv ut xs)
    where
    replace = let ut = [head t] in maybe (M.insert r ut v, tail t, ut) (\n -> (v, t, n)) (M.lookup r v)
      where r = stripWith (\c -> (c == '(') || (c == ')')) x

-- | Strip unneeded whitespace from a signature, e.g. @String -> Map k a -> Int@ will be transformed
-- to @String->Map k a->Int@.
stripSignature :: String -> String
stripSignature = sep "->" . sep "(" . sep ")" . sep "." . sep "=>"
  where
  sep s = join s . map strip . split s  
 
              
              
              
{-
mergeAll' :: (Documents FunctionInfo, Inverted) -> IndexerConfig -> IO (Documents FunctionInfo, Inverted)
mergeAll' (docs, idx) idxConfig = 
    do
    docs2  <- loadFromBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") :: IO (Documents FunctionInfo)
    idx2   <- loadFromBinFile ( (ic_idxPath idxConfig) ++ "-index.bin") :: IO Inverted
    return $! mergeAll docs idx docs2 idx2
-}
{-
-- | build a cache - this will be changed when caches become mergable
buildCache :: String -> [(DocId, URI)] -> IO()
buildCache path l =
  do 
  cache <- createCache (path ++ "-cache.db")
  mapM (cacheDoc cache) l
  return ()
  where 
  cacheDoc :: HolCache c => c -> (DocId, URI) -> IO()
  cacheDoc cache (docId, u) =
    do
    theText <- runX (      readDocument standardReadDocumentAttributes u 
                       >>> getXPathTrees "/tr/td[@class='doc']"
                       >>> deep isText
                       >>> getText
                       >>> unlistA
                       >>> strictA
                     )
    if (length theText > 0) && 
       (length (dropWhile ((==) ' ') (theText)) > 0)
          then putDocText cache "description" docId (theText)
          else return ()
-}              
              
              
              