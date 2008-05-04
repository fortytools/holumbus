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
import           Holumbus.Build.Tokenize
import           Holumbus.Build.XmlFilter
import           Holumbus.Control.MapReduce.Parallel
import           Holumbus.Index.Cache

import           Holumbus.Index.Common
import           Holumbus.Index.Inverted(Inverted, emptyInverted)
import           Holumbus.Index.Documents
import qualified Holumbus.Index.Documents as DOC  
import           Holumbus.Utility

import           Network.URI(unEscapeString)

-- import           Hayoo.Common

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML
import           Text.XML.HXT.Arrow.XmlRegex

main :: IO ()
main 
  = do
    -- ---------------------------------------------------------------------------------------------
       -- Configuration
    traceLevel    <- return 0
    workerThreads <- return 5 
    splitPath     <- return "/home/sms/tmp/split/"
    indexPath     <- return "/home/sms/indexes/hayoo"

{-
    idxConfig     <- return $ mergeIndexerConfigs ic_HXT [ic_Holumbus, ic_GHC_libs]
    buildAnIndex traceLevel workerThreads splitPath idxConfig
    theDocs       <- loadFromBinFile ((ic_idxPath ic_HXT) ++ "-docs.bin") :: IO (Documents FunctionInfo)
    allTmpDocs    <- return $ tmpDocs splitPath theDocs 
-}  
    

    idxConfigs    <- return $    []
--                              ++ [ic_Hayoo]            -- hackage.haskell.org
                              ++ [ic_GHC_libs]         -- all GHC libs
                              ++ [ic_HXT]              -- hxt arrow & filter docs
                              ++ [ic_Holumbus]         -- holumbus framework
--                              ++ [ic_Single]            -- one document for debugging

    -- ---------------------------------------------------------------------------------------------
       -- build indexes and write them to hard disk to save memory
    mapM (buildAnIndex traceLevel workerThreads splitPath) idxConfigs   
       -- merge all indexes and document tables that have been written to the hard disk and create
       -- a document table of all temporary files on the harddisk for caching    
    
    cfg  <- return $ head idxConfigs
    idxConfigs' <- return $ tail idxConfigs

    idx  <- loadFromBinFile ( (ic_idxPath cfg) ++ "-index.bin") :: IO Inverted
    docs <- loadFromBinFile ( (ic_idxPath cfg) ++ "-docs.bin")  :: IO (Documents FunctionInfo)
    
    
    
    (theDocs, theIdx) <- foldM mergeAll' (docs, idx) idxConfigs'    
    
    writeToXmlFile ( indexPath ++ "-docs.xml")  theDocs
    writeToBinFile ( indexPath ++ "-docs.bin")  theDocs
    writeToXmlFile ( indexPath ++ "-index.xml") theIdx
    writeToBinFile ( indexPath ++ "-index.bin") theIdx
    
--    theDocs <- loadFromBinFile ( indexPath ++ "-docs.bin")  :: IO (Documents FunctionInfo)
    
    
    allTmpDocs <- return $ tmpDocs splitPath theDocs     
    

    
    -- ---------------------------------------------------------------------------------------------
    runX (traceMsg 0 (" cacheing  ------------------------------ " ))
    buildCache  indexPath (map (\(i,d) -> (i, uri d)) (IM.toList $ DOC.toMap allTmpDocs))
    return ()
    -- ---------------------------------------------------------------------------------------------

mergeAll' :: (Documents FunctionInfo, Inverted) -> IndexerConfig -> IO (Documents FunctionInfo, Inverted)
mergeAll' (docs, idx) idxConfig = 
    do
    docs2  <- loadFromBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") :: IO (Documents FunctionInfo)
    idx2   <- loadFromBinFile ( (ic_idxPath idxConfig) ++ "-index.bin") :: IO Inverted
    return $! mergeAll docs idx docs2 idx2

buildAnIndex :: Int -> Int -> String -> IndexerConfig -> IO ()
buildAnIndex traceLevel workerThreads splitPath idxConfig
  = do 
    crawlState    <- return (initialCS idxConfig customCrawlFunc)
       -- find available documents and save local copies as configured 
       -- in the IndexerConfig
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl traceLevel workerThreads crawlState
--    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-predocs.xml") (docs)
    -- ---------------------------------------------------------------------------------------------
       -- split the locally saved documents into several small xml files
       -- where each file consists of the documentation for one function
    runX (traceMsg 0 (" splitting ----------------------------- " ))
    splitDocs' <- mapReduce 
                    workerThreads 
                    (getVirtualDocs splitPath)
                    mkVirtualDocList
                    (IM.toList (IM.map uri (DOC.toMap docs)))
    splitDocs  <-  return $! snd (M.elemAt 0 splitDocs')
    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-docs.xml") (splitDocs)
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") (splitDocs)
    -- ---------------------------------------------------------------------------------------------
       -- build an index over the split xml files
    runX (traceMsg 0 (" indexing  ------------------------------ " ))
    localDocs <- return $ tmpDocs splitPath splitDocs
    idx    <- buildIndex workerThreads
                         traceLevel
                         (map (\(i,d) -> (i, uri d)) (IM.toList $ DOC.toMap localDocs))
                         idxConfig
                         emptyInverted 
    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-index.xml") idx
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-index.bin") idx
    -- ---------------------------------------------------------------------------------------------


-- | build a cache - this will be changed when a cache-server is introduced
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
    theText <- runX (      readDocument stdOpts4Reading u 
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

customCrawlFunc :: IOSArrow XmlTree (Maybe FunctionInfo)
customCrawlFunc = constA Nothing

-- | Helper function to replace original URIs by the corresponding pathes for 
--   the locally dumped files
--   <br/>TODO: Move this to a module
tmpDocs :: String -> DOC.Documents a -> DOC.Documents a
tmpDocs tmpPath =  
    DOC.fromMap 
  . (IM.mapWithKey (\docId doc -> Document (title doc) (tmpPath ++ (tmpFile docId (uri doc))) Nothing))
  . DOC.toMap
    
-- -----------------------------------------------------------------------------    
   -- Dirty Stuff for the Creation of Virtual Documents
-- -----------------------------------------------------------------------------    

-- | Function to split a document into virtual documents where virtual document contains the
--   declaration of a function (or data, newtype, ...) and optionally the documentation of this
--   element. This runs as the MAP part of a MapReduce Computation
getVirtualDocs :: String -> Int -> URI -> IO [(Int, (String, String, FunctionInfo))]
getVirtualDocs splitPath docId theUri =         
  runX ( 
      readDocument stdOpts4Reading theUri                       -- 1. read the Document
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
                             &&& fromLA getSignature
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
  getSignature =     removeSourceLinks >>> preFilterSignatures >>> getXPathTreesInDoc theHayooXPath 
                 >>> getText
                 >>^ dropWhile ( /= ':' ) >>^ drop 3 
  getSourceLink :: ArrowXml a => a XmlTree (Maybe String)
  getSourceLink = 
      (      processTopDown ( getChildren `when` (isElem >>> hasName "a" >>> hasAttr "name")  )
        >>> getXPathTreesInDoc "//td[@class='decl']/a/@href/text()"  >>> getText
        >>^ (\a -> if "src/" `isPrefixOf` a then expandURIString a theUri else Nothing)
      ) `withDefault` Nothing


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

-- | removes Source Links from the XmlTree. A Source Link can be identified by the text of an "a" 
--   node but to be more precise it is also checked whether the href-attribute starts with "src".
--   During the tree transformation it might happen, that source links with empty href attributes 
--   are constructed so empty href attributes are also searched and removed if teh text of the "a"
--   node is "Source"
removeSourceLinks :: LA XmlTree XmlTree
removeSourceLinks = 
  processTopDown ( none `when` 
                   (     hasName "a" 
                     >>> hasAttrValue "href" (\a -> "src/" `isPrefixOf` a || length a == 0) 
                     />  hasText (== "Source") )
                 )      



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

removeElementsByXPath :: ArrowXml a => String -> a XmlTree XmlTree 
removeElementsByXPath xpath =
  case xpath of
       "//td[@class='rdoc']"  ->  processTopDown (none `when` (isElem >>> hasName "td" >>> hasAttrValue "class" (== "rdoc")))
       otherwise              ->  none


processDataTypeAndNewtypeDeclarations :: LA XmlTree XmlTree
processDataTypeAndNewtypeDeclarations 
  = processTopDown
      ( ( mkTheElem $<<<< (     getTheName
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




      
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
-- -----------------------------------------------------------------------------    
   -- Crawler & Indexer Configuration
-- -----------------------------------------------------------------------------    
ic_Single :: IndexerConfig
ic_Single = mkIndexerConfig
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

ic_Hayoo :: IndexerConfig
ic_Hayoo = mkIndexerConfig
    [ "http://hackage.haskell.org/packages/archive/pkg-list.html" ]        -- ic_startPages     :: [URI]
    (Just "/home/sms/tmp/hayoo/")                                          -- ic_tmpPath        :: String
    "/home/sms/indexes/hayoo"                                              -- ic_idxPath        :: String
    ccs_Hayoo                                                              -- ic_contextConfigs :: [ContextConfig]
    stdOpts4Reading
    [ "http://hackage.haskell.org/" ]                                      -- allow
    [ "/src/", "/trac/", ".tar.gz$", ".cabal$", ".pdf$", "/doc-index-"     -- deny
    , "/logs/failures" ]

ic_Holumbus :: IndexerConfig
ic_Holumbus =  mkIndexerConfig
    [ "http://www.holumbus.org/docs/develop/" ]      -- ic_startPages :: [URI]
    (Just "/home/sms/tmp/holumbus_docs/")            -- ic_tmpPath    :: String
    "/home/sms/indexes/holumbus"                     -- ic_idxPath    :: String
    ccs_Hayoo                                   
    stdOpts4Reading
    [ "holumbus.org/docs/develop" ]
    [ "/src/"]      

ic_GHC_libs :: IndexerConfig         -- works
ic_GHC_libs =  mkIndexerConfig
    [ "http://www.haskell.org/ghc/docs/latest/html/libraries/"
    , "http://www.haskell.org/ghc/docs/latest/html/libraries/index.html"
    , "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Exception.html"
    ]                                                                        -- ic_startPages :: [URI]
    (Just "/home/sms/tmp/ghc_libs/" )                                        -- ic_tmpPath    :: String
    "/home/sms/indexes/ghclibs"                                                -- ic_idxPath    :: String
    ccs_Hayoo
    stdOpts4Reading
    [ "^http://www.haskell.org/ghc/docs/latest/html/" ]
    [ "/src/", "^http://www.haskell.org/ghc/docs/latest/html/libraries/doc-index.html"]  -- this page causes problems 


ic_HXT :: IndexerConfig
ic_HXT =  mkIndexerConfig
    [ "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc/index.html"
    ]
    (Just "/tmp/")
    "/home/sms/indexes/hxt"
    ccs_Hayoo
    stdOpts4Reading
    [ "^http://www.fh-wedel.de/~si/HXmlToolbox/hdoc"]
    [ "/src/"]
  
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
              (\s -> (s == "type" || s == "class" || s == "data")) 

ccHayooPartialName :: ContextConfig
ccHayooPartialName = ContextConfig "partial" (fromLA preFilterSignatures) theHayooXPath partTokenize (\s -> (s == "type" || s == "class" || s == "data")) 
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
ccHayooSignature = ContextConfig "signature" (fromLA preFilterSignatures) theHayooXPath (\a -> [getSignature a]) (\s -> length s == 0)

getSignature :: String -> String
getSignature s = if "=>" `isInfixOf` s
                      then stripSignature (drop 3 (dropWhile ((/=) '=') s)) -- TODO split!
                      else stripSignature (drop 3 (dropWhile ((/=) ':') s))


ccHayooNormalizedSignature :: ContextConfig
ccHayooNormalizedSignature = ContextConfig "normalized" (fromLA preFilterSignatures) theHayooXPath
  (\s -> [normalizeSignature (getSignature s)]) (\s -> length s ==  0)

-- | Configruation for description context.
ccHayooDescription :: ContextConfig 
ccHayooDescription 
  = ContextConfig { cc_name        = "description" 
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_XPath       = "//td[@class='doc']//text()"
                  , cc_fTokenize   = map (stripWith (=='.')) . (parseWords isWordChar)
                  , cc_fIsStopWord = (\s -> length s < 2)
                  }

ccHaddockSignatures :: ContextConfig
ccHaddockSignatures 
  = ContextConfig { cc_name        = "signatures"
                  , cc_preFilter   = fromLA preFilterSignatures
                  , cc_XPath       = theHayooXPath
                  , cc_fTokenize   = (\a -> [a])
                  , cc_fIsStopWord = const False     
                  }
                  
ccModule :: ContextConfig
ccModule = ContextConfig "module" (fromLA preFilterSignatures) "/module/text()" (\a -> [a]) (const False)     

ccHierarchy :: ContextConfig
ccHierarchy = ContextConfig "hierarchy" (fromLA preFilterSignatures) "/module/text()" (split ".") (const False)     
            
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

flattenAllElements :: LA XmlTree XmlTree
flattenAllElements 
    = processTopDown ( getChildren
                       `when`
                       isElem
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
  
  
  -- | some standard options for the readDocument function
stdOpts4Reading :: [(String, String)]
stdOpts4Reading = []
  ++ [ (a_parse_html, v_1)]
  ++ [ (a_issue_warnings, v_0)]
  ++ [ (a_tagsoup, v_1) ]
  ++ [ (a_use_curl, v_1)]
  ++ [ (a_options_curl, "-L")] --"--user-agent HolumBot/0.1 --location")]   
  
  
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
 
              