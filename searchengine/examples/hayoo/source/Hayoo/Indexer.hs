-- ----------------------------------------------------------------------------

{- |
  Module     : HayooIndexer
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: untested
  Version    : 0.2.3

  Indexer for the Haskell API Documentation Search Engine Hayoo!

  Current scope:
    - <http://hackage.haskell.org/>
    - <http://www.haskell.org/gtk2hs/docs/current/>
  
  TODO: 
  - Include class, data, ... descriptions.

-}

-- ----------------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

module Main where

-- import           HayooConfig

import           Control.Monad hiding (join, when)
import qualified Control.Monad as CM (when)

import           Data.Binary
import           Data.Char
import           Data.List
import           Data.Maybe

import           qualified Data.IntMap as IM
import           qualified Data.Map    as M

import           Holumbus.Build.Config
import           Holumbus.Build.Crawl
import           Holumbus.Build.Index

import           Holumbus.Control.MapReduce.Parallel
-- import           Holumbus.Index.Cache

import           Holumbus.Index.Common
import           Holumbus.Index.Inverted.Memory(emptyInverted)
import           Holumbus.Index.Cache
import           Holumbus.Index.Documents
import           Holumbus.Utility

import           Network.URI(unEscapeString)

import           System.Directory
import           System.Environment
import	         System.Exit

import           Text.XML.HXT.Arrow     
import           Text.XML.HXT.Arrow.XmlRegex
import           Text.Regex

main :: IO ()
main 
  = do

    argv <- getArgs
    if length argv > 1 then exitWith (ExitFailure (-1)) else return ()
    if (length argv == 1) && (not ((argv !! 0) `elem` ["-c", "-s", "-i"])) then exitWith (ExitFailure (-1)) else return ()
    -- ---------------------------------------------------------------------------------------------
    --   Configuration
    -- ---------------------------------------------------------------------------------------------
    dir <- getHomeDirectory

    let traceLevel    = 0
        workerThreads = 8 
        docsPerCrawl  = 256
        indexPath     = dir ++ "/indexes/hayoo"
--        idxConfig     = ic_HTTP { ic_indexPath = dir ++ "/indexes/hxt" }
	idxConfig     = ic_Hayoo { ic_indexPath = indexPath }
	
        additionalConfig = ic_Hayoo_additional 
                           { ic_indexPath    = indexPath
--                           , ic_startPages   = ["http://www.haskell.org/gtk2hs/docs/current/Graphics-UI-Gtk-General-Drag.html"]
--                           , ic_fCrawlFilter = const False
                           } 
        
	crawlerState'  = (initialCrawlerState idxConfig emptyDocuments customCrawlFunc)
	crawlerState  = crawlerState' {
	  cs_fGetReferences = appendLinkUnifier (cs_fGetReferences crawlerState')
	}

    
      -- we don't want to shipwreck because of missing dirs
    createDirectoryIfMissing True  ((fromJust (ic_tempPath idxConfig)) ++ "split/")
    createDirectoryIfMissing True  (fromJust (ic_tempPath idxConfig))
    createDirectoryIfMissing False indexPath
    e <- doesFileExist (indexPath ++ "-cache.db")      -- remove cache if already existing
    CM.when e (removeFile (indexPath ++ "-cache.db"))
    
    
    -- ---------------------------------------------------------------------------------------------
    -- crawl or load documents from file 
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    hayooDocs <- 
       -- find available documents and save local copies as configured 
       -- in the IndexerConfig
      if (length argv) == 0 || (argv !! 0) == "-c"
      then 
	do
	  runX (traceMsg 0 ("           (1) Hackage " ))
	  hackageCrawled <- crawl traceLevel workerThreads docsPerCrawl crawlerState 
	  let hackageDocs =  filterDocuments -- TODO clean this up
                         ( isPrefixOf "http://hackage.haskell.org/packages/archive/" . uri) $
                       filterDocuments 
                         (\d -> (not $ isSuffixOf "pkg-list.html" $ uri d))  $ 
                       filterDocuments 
                         (\d -> (not $ isSuffixOf "recent.html"  $ uri d )) 
                       hackageCrawled

	      hackageDocs'  = filterDocuments (hackageFilter . uri) hackageDocs
	      hackageLatest = filterDocuments (isLatestVersion (getVersions hackageDocs')) hackageDocs'

	  runX (traceMsg 0 ("           (2) Additional libraries " ))
	  let additionalState = initialCrawlerState additionalConfig emptyDocuments customCrawlFunc
	  additionalDocs <- crawl traceLevel workerThreads docsPerCrawl additionalState
      
	  let hayooDocs' = snd $ mergeDocs hackageLatest additionalDocs
    	  writeToXmlFile ( (ic_indexPath idxConfig) ++ "-predocs.xml") hayooDocs'
	  writeToBinFile ( (ic_indexPath idxConfig) ++ "-predocs.bin") hayooDocs'
	  return hayooDocs'
      else (loadFromBinFile ( (ic_indexPath idxConfig) ++ "-predocs.bin") :: IO (Documents FunctionInfo))

  
--    hayooDocs <- loadFromBinFile  ( (ic_indexPath idxConfig) ++ "-predocs.bin") :: IO (Documents FunctionInfo)
    hayooDocsSize <- return $! sizeDocs hayooDocs
    

    -- ---------------------------------------------------------------------------------------------
    -- split files or load documents from file
    runX (traceMsg 0 (" splitting ----------------------------- " ))
       -- split the locally saved documents into several small xml files
       -- where each file contains declaration & documentation of one function
    splitDocs <-
      if (length argv) == 0 || (argv !! 0) == "-s"
      then
	do
	  splitDocs'' <- mapReduce 
			workerThreads 
			(getVirtualDocs ((fromJust $ ic_tempPath idxConfig) ++ "split/"))
			mkVirtualDocList
			(IM.toList (IM.map uri (toMap hayooDocs)))
	  splitDocs'  <-  return $! snd (M.elemAt 0 splitDocs'')
	  writeToXmlFile ( indexPath ++ "-docs.xml") (splitDocs')
	  writeToBinFile ( indexPath ++ "-docs.bin") (splitDocs')
	  return splitDocs'
      else
	loadFromBinFile ( indexPath ++ "-docs.bin") :: IO (Documents FunctionInfo)

    splitDocsSize <- return $! sizeDocs splitDocs

    -- ---------------------------------------------------------------------------------------------
    -- build an index over the split xml files if wanted
    
    if (length argv) == 0 || (argv !! 0) == "-i"
      then
      do
	runX (traceMsg 0 (" indexing  ------------------------------ " ))
	localDocs <- return $ tmpDocs ((fromJust $ ic_tempPath idxConfig) ++ "split/") splitDocs
    	cache     <- createCache ((ic_indexPath idxConfig) ++ "-cache.db")
	idx       <- buildIndex workerThreads traceLevel localDocs idxConfig
                            emptyInverted (Just cache)
	writeToXmlFile ( indexPath ++ "-index.xml") idx
	writeToBinFile ( indexPath ++ "-index.bin") idx
    
	  -- ---------------------------------------------------------------------------------------------
	  -- display some statistics
	putStrLn "---------------------------------------------------------------"
	putStrLn $ "Documents: " ++ show hayooDocsSize
	putStrLn $ "Split documents: " ++ show splitDocsSize
	putStrLn $ "Unique Words: " ++ show (sizeWords idx)    
	putStrLn "---------------------------------------------------------------"
	return ()
      else
	return ()


appendLinkUnifier :: ArrowXml a' => a' XmlTree [URI] -> a' XmlTree [URI]
appendLinkUnifier a = 
  a >>> arr (map (\s -> subRegex (mkRegex "http://hackage.haskell.org/packages/archive/([^/]*)/[^/]*/doc/html/(.*)") 
				 s
				 "http://hackage.haskell.org/packages/archive/\\1/latest/doc/html/\\2"))

packageFromURI :: String -> String
packageFromURI u = if "http://www.haskell.org/gtk2hs/docs/current/" `isPrefixOf` u
                     then "gtk2hs"
                     else if "http://hackage.haskell.org/packages/archive/" `isPrefixOf` u
                            then (split "/" u) !! 5
                            else "unknown package"



isLatestVersion :: M.Map String String -> Document a -> Bool
isLatestVersion m d = ver == M.findWithDefault "0" pkg m
  where 
  pkg = (split "/" (uri d)) !! 5
  ver = (split "/" (uri d)) !! 6

getVersions :: Binary a => Documents a -> M.Map String String
getVersions docs = foldl' (\m d -> M.insertWith cmp (pkg d) (ver d) m) M.empty
                          (map snd $ IM.toList $ toMap docs)
  where 
  cmp v1 v2 = if v1 >= v2 then v1 else v2
  pkg d     = (split "/" (uri d)) !! 5
  ver d     = (split "/" (uri d)) !! 6
          

hackageFilter :: URI -> Bool
hackageFilter = simpleCrawlFilter [ "http://hackage.haskell.org/" ]                  -- allow
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
                                         , "Types-Data-Num-Decimal-Literals.html"
                                         ]


-- -------------------------------------------------------------------------------------------------

-- | dummy function for the custom information in the Documents. This is used in the crawl phase
--   of the index generation. The real custom information is computed during the split phase where
--   every document is split up into pseude-documents which contain exactly one function
customCrawlFunc :: IOSArrow XmlTree (Maybe FunctionInfo)
customCrawlFunc = constA Nothing

    
-- -----------------------------------------------------------------------------    
--   Creation of Virtual Documents
-- -----------------------------------------------------------------------------    

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
     >>> writeDocument [("a_output_xml", "1")] ((splitPath ++ tmpFile docId theUri) ++ (escape (theLinkPrefix ++ theTitle)))
     >>> (     constA theTitle 
           &&& constA (theUri ++ theLinkPrefix ++ theTitle)
           &&& constA (FunctionInfo theModule theSignature thePackage (theSourceURI))
         )
     >>^ (\(a,(b,c)) -> (a,b,c)) 
  getTheSignature =     removeSourceLinks 
                    >>> fromLA ( listA ( xshow ( (cc_fExtract ccHayooSignature) >>> getTexts)))
                    >>^ concat >>^ dropWhile ( /= ':' ) >>^ drop 3                              
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

-- | The REDUCE phase of the virtual document creation MapReduce computation.
--   The virtual Documents from the MAP phase are collected and a new Documents data is created
mkVirtualDocList :: Int -> [(String, String, FunctionInfo)] -> IO (Maybe (Documents  FunctionInfo))
mkVirtualDocList _ vDocs
  = return $! Just $ foldl' (\d r -> snd (insertDoc d r)) 
                             emptyDocuments 
                            (map (\(t, u, fi) -> Document t u (Just fi)) vDocs)
            

-- -----------------------------------------------------------------------------

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
                        , mkelem "a" [sattr "href" s] [constA " " >>> mkText] -- [constA "Source" >>> mkText]
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


-- -----------------------------------------------------------------------------
   -- Uwe Schmidts Regex-Arrow stuff
-- -----------------------------------------------------------------------------

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
    = listA (hasName "tr" /> hasName "td" /> hasName "a"  >>> getAttrValue "name")>>. (take 1) . concat
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
groupDeclDoc ts   
      = scanRegexA declDoc ts >>>
        mkelem "tr"
        [ sattr "class" "decl"
        , attr  "id" (unlistA >>> getDeclName >>> mkText)
        ] [unlistA >>> getChildren]

isArg :: LA XmlTree XmlTree
isArg = hasName "tr" />  hasName "td" >>> hasAttrValue "class" (== "arg")


removeSpacers :: LA XmlTree XmlTree
removeSpacers =
  processTopDown
          (  none
            `when`
             (hasName "tr" /> hasName "td" >>> hasAttrValue "class" (\a ->  ( a == "s15" || a == "s8" ) ) )
          )




                
              
              
-- -----------------------------------------------------------------------------    
   -- Crawler & Indexer Configuration
-- -----------------------------------------------------------------------------    
ic_Single :: IndexerConfig
ic_Single
  = IndexerConfig
    { ic_startPages     = -- [ "http://hackage.haskell.org/packages/archive/markov-chain/0.0.2/doc/html/Data-MarkovChain.html"
                          [ "http://hackage.haskell.org/packages/archive/bytestring/0.9.1.0/doc/html/Data-ByteString.html"]
--                          ["http://holumbus.fh-wedel.de/docs/develop/Holumbus-Searchengine/Holumbus-Index-Common.html"]
    , ic_tempPath        = Just "/tmp/hayoo-files/"
    , ic_indexPath        = "/home/sms/indexes/exception"
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs = ccs_Hayoo
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = const False
    }

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
    { ic_startPages      = [ "http://hackage.haskell.org/packages/archive/pkg-list.html" 
--                           , "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html"
                           ]
    , ic_tempPath        = Just "/tmp/holumbus/hayoo/"
    , ic_indexPath       = "/tmp/hayoo"  -- will be overwritten in main-function
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs  = ccs_Hayoo
    , ic_readAttributes  = standardReadDocumentAttributes
    , ic_fCrawlFilter    = hackageFilter
    }
    
ic_Hayoo_additional :: IndexerConfig
ic_Hayoo_additional
  = ic_Hayoo
    { ic_startPages      = [ "http://www.haskell.org/gtk2hs/docs/current/"
                           ]
    , ic_fCrawlFilter    = simpleCrawlFilter ["http://www.haskell.org/gtk2hs/docs/current/"] []
    }                            

theHayooXPath :: String
theHayooXPath =  "//td[@class='decl']"     
  
ccs_Hayoo :: [ContextConfig]
ccs_Hayoo = [  ccModule
            , ccHierarchy
            , ccHayooName     
            , ccHayooPartialName 
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
                  , cc_fTokenize   = \a -> [a]
                  , cc_fIsStopWord = (flip elem) ["unknownpackage"]
                  , cc_addToCache  = False
                  }
                              
getSignature :: String -> String
getSignature s = if "=>" `isInfixOf` s  then stripSignature $ drop 3 $ dropWhile ((/=) '=') s
                                        else stripSignature $ drop 3 $ dropWhile ((/=) ':') s


-- | TODO some of this is redundant, some important. find out what and why and simplify
preFilterSignatures :: LA XmlTree XmlTree
preFilterSignatures -- = removeDeclbut
-- =  this
    =     flattenElementsByType "b"
--      >>> flattenElementsByType "a"
      >>> flattenElementsByType "span"
--      >>> flattenElementsByType "tt"
      >>> flattenElementsByType "em"
      >>> flattenElementsByType "p"
      >>> flattenElementsByType "pre"
      >>> removeDeclbut  -- redundant !?

flattenElementsByType :: String -> LA XmlTree XmlTree
flattenElementsByType s = processTopDown ( getChildren `when` hasName s )

removeDeclbut :: ArrowXml a => a XmlTree XmlTree
removeDeclbut = processTopDown (  none `when` isDeclbut )
  where isDeclbut = isElem  >>> hasName "td" >>> hasAttrValue "class" (isPrefixOf "declbut") 

-- -------------------------------------------------------------------------------------------------
-- stuff from Hayoo.Common, will be removed soon

-- | Additional information about a function.
data FunctionInfo = FunctionInfo 
  { moduleName :: String      -- ^ The name of the module containing the function, e.g. Data.Map
  , signature :: String       -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
  , package   :: String
  , sourceURI :: Maybe String -- ^ An optional URI to the online source of the function.
  } 
  deriving (Show, Eq)

instance XmlPickler FunctionInfo where
  xpickle = xpWrap (\(m, s, p, r) -> FunctionInfo m s p r, \(FunctionInfo m s p r) -> (m, s, p, r)) xpFunction
    where
    xpFunction = xp4Tuple xpModule xpSignature xpPackage xpSource
      where -- We are inside a doc-element, therefore everything is stored as attribute.
      xpModule = xpAttr "module" xpText0
      xpSignature = xpAttr "signature" xpText0
      xpPackage = xpAttr "package" xpText0
      xpSource = xpOption (xpAttr "source" xpText0)

instance Binary FunctionInfo where
  put (FunctionInfo m s p r) = put m >> put s >> put p >> put r
  get = liftM4 FunctionInfo get get get get
  
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
stripSignature = sep "->" . lsep "(" . rsep ")" . sep "." . sep "=>"
  where
  sep s = join s . map strip . split s
  lsep s = join s . map stripl . split s
  rsep s = join s . map stripr . split s 
 


ic_HTTP :: IndexerConfig
ic_HTTP 
  = IndexerConfig
    { ic_startPages     = [ "http://hackage.haskell.org/packages/archive/HTTP/4000.0.7/doc/html/Network-Browser.html"]
    , ic_tempPath        = Just "/tmp/"
    , ic_indexPath        = "/home/sms/indexes/http"
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs = ccs_Hayoo
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "http://hackage.haskell.org/packages/archive/HTTP/4000.0.7/doc/html/"]
                                            [ "/src/"]
    }  

            
ic_Holumbus :: IndexerConfig
ic_Holumbus 
  =  IndexerConfig
     { ic_startPages     = [ "http://holumbus.fh-wedel.de/docs/Holumbus-Searchengine/doc-index.html" ]
     , ic_tempPath       = Just "/home/sms/tmp/holumbus_docs/"
     , ic_indexPath      = "/home/sms/indexes/holumbus"
     , ic_indexerTimeOut = 10 * 60 * 1000000
     , ic_contextConfigs = ccs_Hayoo                                   
     , ic_readAttributes = standardReadDocumentAttributes
     , ic_fCrawlFilter   = simpleCrawlFilter [ "http://holumbus.fh-wedel.de/docs/" ] [ "/src/"]
     }      

ic_GHC_libs :: IndexerConfig         
ic_GHC_libs
 = IndexerConfig 
   { ic_startPages      = [ "http://www.haskell.org/ghc/docs/latest/html/libraries/"
                          , "http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Exception.html"
                          ]
   , ic_tempPath         = Just "/home/sms/tmp/ghc_libs/"
   , ic_indexPath         = "/home/sms/indexes/ghclibs"
   , ic_indexerTimeOut  = 10 * 60 * 1000000
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
    , ic_tempPath        = Just "/tmp/"
    , ic_indexPath        = "/home/sms/indexes/hxt"
    , ic_indexerTimeOut  = 10 * 60 * 1000000
    , ic_contextConfigs = ccs_Hayoo
    , ic_readAttributes = standardReadDocumentAttributes
    , ic_fCrawlFilter   = simpleCrawlFilter [ "^http://www.fh-wedel.de/~si/HXmlToolbox/hdoc"]
                                            [ "/src/"]
    }              

