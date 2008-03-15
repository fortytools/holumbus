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

import           Data.Binary
import           Control.Monad hiding (join)

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
import           Holumbus.Index.Inverted(emptyInverted)
import qualified Holumbus.Index.Documents as DOC  
import           Holumbus.Utility



-- import HayooHelper

import           Text.XML.HXT.Arrow     -- import all stuff for parsing, validating, and transforming XML
import           Text.XML.HXT.Arrow.XmlRegex

-- | Additional information about a function.
data FunctionInfo = FunctionInfo 
  { moduleName :: String       -- ^ The name of the module containing the function, e.g. Data.Map
  , signature  :: String       -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
  , sourceURI  :: Maybe String -- ^ An optional URI to the online source of the function.
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

main :: IO ()
main 
  = do
    traceLevel    <- return 1
    workerThreads <- return 5 
    idxConfig     <- return ic_GHC_libs
--    idxConfig     <- return ic_HolumbusDocs
--    idxConfig     <- return ic_Hayoo
    splitPath     <- return "/home/sms/tmp/holumbus_docs/split/"
    crawlState    <- return (initialCS idxConfig customCrawlFunc)
    -- -------------------------------------------------------------------------
       -- find the available documents and save local copies as configured 
       -- in the IndexerConfig
    runX (traceMsg 0 (" crawling  ----------------------------- " ))
    docs       <- crawl traceLevel workerThreads crawlState
    -- -------------------------------------------------------------------------
       -- split the locally saved documents into several small xml files
       -- where each file consists of the documentation for one function
    runX (traceMsg 0 (" splitting ----------------------------- " ))
    splitDocs' <- mapReduce 
                    workerThreads 
                    (getVirtualDocs splitPath)
                    mkDocList
                    (IM.toList (IM.map uri (DOC.toMap docs)))
    splitDocs  <-  return $! snd (M.elemAt 0 splitDocs')
    writeToXmlFile ( (ic_idxPath idxConfig) ++ "-docs.xml") (splitDocs)
    writeToBinFile ( (ic_idxPath idxConfig) ++ "-docs.bin") (splitDocs)
    -- -------------------------------------------------------------------------
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
    -- -------------------------------------------------------------------------
    buildCache  (ic_idxPath idxConfig)
                (map (\(i,d) -> (i, uri d)) (IM.toList $ DOC.toMap localDocs))
    return ()
    -- -------------------------------------------------------------------------

buildCache :: String -> [(DocId, URI)] -> IO()
buildCache path l =
  do 
  cache <- createCache (path ++ "-cache.db")
  mapM (cacheDoc cache) l
  return ()
  where 
  cacheDoc :: HolCache c => c -> (DocId, URI) -> IO()
  cacheDoc cache (docId, uri) =
    do
    theText <- runX (      readDocument stdOpts4Reading uri 
                       >>> flattenElementsByType "tt"
                       >>> flattenElementsByType "em"
                       >>> flattenElementsByType "a"
                       >>> getXPathTreesInDoc "/tr/td[@class='doc']//text()"
                       >>> getText
                       >>> unlistA
                     )
    if (length theText > 0) && 
       (length (dropWhile ((==) ' ') (theText)) > 0)
          then putDocText cache "description" docId (theText)
          else return ()

customCrawlFunc :: IOSArrow XmlTree (Maybe FunctionInfo)
customCrawlFunc = constA Nothing


-- ld = "/home/sms/tmp/holumbus_docs/split/http-_-__www.holumbus.org_docs_develop_Holumbus-Utility.html-_-v-_-join"

-- | Helper function to replace original URIs by the corresponding pathes for 
--   the locally dumped files
--   <br/>TODO: Move this to a module
tmpDocs :: String -> DOC.Documents a -> DOC.Documents a
tmpDocs tmpPath =  
    DOC.fromMap 
  . (IM.mapWithKey (\docId doc -> Document (title doc) (tmpPath ++ (tmpFile docId (uri doc))) Nothing))
  . DOC.toMap
    
-- -----------------------------------------------------------------------------    
   -- Crawler & Indexer Configuration
-- -----------------------------------------------------------------------------    
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

ic_HolumbusDocs :: IndexerConfig
ic_HolumbusDocs =  mkIndexerConfig
    [ "http://www.holumbus.org/docs/develop/" ]      -- ic_startPages :: [URI]
    (Just "/home/sms/tmp/holumbus_docs/")            -- ic_tmpPath    :: String
    "/home/sms/indexes/hayoo"                -- ic_idxPath    :: String
    ccs_Hayoo                                   
    stdOpts4Reading
    [ "holumbus.org/docs/develop" ]
    [ "/src/"]      

ic_GHC_libs :: IndexerConfig         -- works
ic_GHC_libs =  mkIndexerConfig
    [ "http://www.haskell.org/ghc/docs/latest/html/libraries/" ]      -- ic_startPages :: [URI]
    (Just "/home/sms/tmp/ghc_libs/" )                                                   -- ic_tmpPath    :: String
    "/home/sms/indexes/hayoo"                                                -- ic_idxPath    :: String
    ccs_Hayoo
    stdOpts4Reading
    [ "^http://www.haskell.org/ghc/docs/latest/html/" ]
    [ "/src/", "^http://www.haskell.org/ghc/docs/latest/html/libraries/doc-index.html"]  -- this page causes problems 

ccs_Hayoo :: [ContextConfig]
ccs_Hayoo = [ ccHayooName     
            , ccHayooPartialName 
            , ccHayooSignature 
            , ccHayooNormalizedSignature
            , ccHayooDescription
            ]
            
theHayooXPath :: String
theHayooXPath =  "/tr/td[@class='decl']//text()" 
-- theHayooXPath =  "/tr/td[@class='decl']/text()" 
            
ccHayooName :: ContextConfig
ccHayooName = ContextConfig "name" (preFilterSignatures) theHayooXPath  
              (\a -> [takeWhile ( (/=) ' ') (dropWhile ( (==) ' ') a)])
              (\s -> (s == "type" || s == "class" || s == "data")) 

ccHayooPartialName :: ContextConfig
ccHayooPartialName = ContextConfig "partial" preFilterSignatures theHayooXPath partTokenize (\s -> (s == "type" || s == "class" || s == "data")) 
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
ccHayooSignature = ContextConfig "signature" preFilterSignatures theHayooXPath (\a -> [getSignature a]) (\s -> length s == 0)

getSignature :: String -> String
getSignature s = if "=>" `isInfixOf` s
                      then spaceless (drop 3 (dropWhile ((/=) '=') s)) -- TODO split!
                      else spaceless (drop 3 (dropWhile ((/=) ':') s))
    where
    spaceless :: String -> String
    spaceless = filter (not . isSpace)

ccHayooNormalizedSignature :: ContextConfig
ccHayooNormalizedSignature = ContextConfig "normalized" preFilterSignatures theHayooXPath
  (\s -> [normalizeSignature (getSignature s)]) (\s -> length s ==  0)

ccHayooDescription :: ContextConfig 
ccHayooDescription = ContextConfig "description" preFilterSignatures "//td[@class='doc']//text()" (parseWords isWordChar) (\s -> length s < 2)

ccHaddockSignatures :: ContextConfig
ccHaddockSignatures = ContextConfig "signatures" preFilterSignatures theHayooXPath (\a -> [a]) (const False)     

preFilterSignatures :: ArrowXml a => a XmlTree XmlTree
preFilterSignatures
    =     flattenElementsByType "b"
      >>> flattenElementsByType "a"
      >>> flattenElementsByType "span"
      >>> removeDeclbut
      

{-flattenDeclbut :: ArrowXml a => a XmlTree XmlTree
flattenDeclbut =
       processTopDown 
          (  getChildren
            `Text.XML.HXT.Arrow.when`
             isDeclbut
          )-}


removeDeclbut :: ArrowXml a => a XmlTree XmlTree
removeDeclbut =
       processTopDown 
          (  none
            `Text.XML.HXT.Arrow.when`
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

-- -----------------------------------------------------------------------------    
   -- Dirty Stuff for the Creation of Virtual Documents
-- -----------------------------------------------------------------------------    

getVirtualDocs :: String -> Int -> URI -> IO [(Int, (String, String, FunctionInfo))]
getVirtualDocs splitPath docId theUri =         
  runX ( 
      readDocument stdOpts4Reading theUri
  >>> topdeclToDecl
  >>> fromLA (processChildren processDocumentRootElement)
  >>> (makeVirtualDocs theUri $< (getXPathTrees "/html/head/title/text()" >>> getText))
  >>> (constA 42 &&& this)
  )    
  where
  makeVirtualDocs theUri moduleName =
           getXPathTrees "//tr[@class='decl' and @id]"
    >>> (  mkVirtual $<<<< (     (     getXPathTreesInDoc "/tr[@class='decl']/@id/text()"
                                   >>> getText
                                 )
                             &&& constA moduleName
                             &&& getSignature
                             &&& (getSource >>> arr (\a -> fromMaybe a $ expandURIString a theUri))
                           )      
         )
  mkVirtual theTitle moduleName signature sourceURI =     
         root [] [this]
     >>> processTopDown (none `Text.XML.HXT.Arrow.when` (isElem >>> hasName "a" >>> hasAttrValue "href" (isPrefixOf "src/" )))    
     >>> writeDocument [("a_output_xml", "1")] ((splitPath ++ tmpFile docId theUri) ++ (escape ("#v:" ++ theTitle)))
     >>> (     constA theTitle 
           &&& constA (theUri ++ "#v:" ++ theTitle)
           &&& constA (FunctionInfo moduleName signature (Just sourceURI))
         )
     >>> arr (\(a,(b,c)) -> (a,b,c)) 
  getSignature =
--        processTopDown (none `Text.XML.HXT.Arrow.when` (isElem >>> hasName "a" >>> hasAttrValue "href" (isPrefixOf "src/" )))    
        preFilterSignatures
    >>> 
    getXPathTreesInDoc theHayooXPath
    >>> getText
    >>> arr (\s -> (drop 3 (dropWhile ((/=) ':') s)))
    >>> arr (\s -> if "Source" `isSuffixOf` s
                     then reverse $ (drop 6) $ reverse s
                     else s)
  getSource = 
        processTopDown ( getChildren 
                         `Text.XML.HXT.Arrow.when` 
                         (isElem >>> hasName "a" >>> hasAttr "name")
                       )
    >>> getXPathTreesInDoc "//td[@class='decl']/a/@href/text()"
    >>> getText

{-topdeclToDecl :: ArrowXml a => a XmlTree XmlTree
topdeclToDecl  
    = processTopDown
        ( (   mkelem "td"
                [ sattr "class" "decl"]
--                [ getChildren >>> removeDeclbut >>> flattenDeclbar ]
                [ getChildren >>> removeDeclbut >>> flattenDeclbar ]
          )
          `Text.XML.HXT.Arrow.when`
          ( isElem >>> hasName "td" >>> hasAttrValue "class" (== "topdecl") )
        )-}

topdeclToDecl :: ArrowXml a => a XmlTree XmlTree
topdeclToDecl  
    = processTopDown
        ( (getChildren >>> getChildren >>> getChildren)
          `Text.XML.HXT.Arrow.when`
          (isElem >>> hasName "table" >>> hasAttrValue "class" (== "declbar"))
        )
      >>>
      processTopDown
        ( (   mkelem "td"
                [ sattr "class" "decl"]
                [ getChildren ]
          )
          `Text.XML.HXT.Arrow.when`
          ( isElem >>> hasName "td" >>> hasAttrValue "class" (== "topdecl") )
        ) 
 
flattenDeclbar :: ArrowXml a => a XmlTree XmlTree
flattenDeclbar 
  = processTopDown
    (  (getChildren >>> getChildren >>> getChildren -- >>> getChildren
      )
      `Text.XML.HXT.Arrow.when`
      (     isElem >>> hasName "table" >>> hasAttrValue "class" (== "declbar") 
      ) 
    )

foo = "/home/sms/tmp/holumbus_docs/http%3a%2f%2fwww%2eholumbus%2eorg%2fdocs%2fdevelop%2fHolumbus%2dIndex%2dInverted%2ehtml"  

mkDocList :: Int -> [(String, String, FunctionInfo)] -> IO (Maybe (DOC.Documents  FunctionInfo))
mkDocList _ vDocs
  = do 
    return $! Just $ foldl' (\d r -> snd (insertDoc d r)) 
                             DOC.emptyDocuments 
                            (map (\(theTitle, theUri, functionInfo) -> Document theTitle theUri (Just functionInfo)) vDocs)
            


-- -----------------------------------------------------------------------------
   -- Uwe Schmidts Regex-Arrow stuff
-- -----------------------------------------------------------------------------

processDocumentRootElement  :: LA XmlTree XmlTree
processDocumentRootElement
    = processXPathTrees
      (replaceChildren (processTableRows (getChildren >>> declAndDocChildren)))
      "/html/body/table"

declAndDocChildren  :: LA XmlTree XmlTree
declAndDocChildren  = (isDecl <+> isDoc) `guards` this

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

getDeclName     :: LA XmlTree String
getDeclName
    = listA (hasName "tr" /> hasName "td" /> hasName "a" >>> getAttrValue "name")>>. concat
      >>> (arr $ drop 4)

processTableRows      :: LA XmlTree XmlTree -> LA XmlTree XmlTree
processTableRows ts
    = groupDeclDoc (remLeadingDocElems ts) {- >>> prune 3 -}

-- regex for a leading class="doc" row

leadingDoc    :: XmlRegex
leadingDoc    = mkStar (mkPrimA isDoc)

-- regex for a class="decl" class="doc" sequence

declDoc     :: XmlRegex
declDoc     = mkSeq (mkPrimA isDecl) leadingDoc

-- remove a leading class="doc" row this does not form a declaration
-- split the list of trees and throw away the first part

remLeadingDocElems  :: LA XmlTree XmlTree -> LA XmlTree XmlTree
remLeadingDocElems ts   = (splitRegexA leadingDoc ts >>^ snd) >>> unlistA

-- group the "tr" trees for a declaration together, build a "tr class="decl"" element and
-- throw the old "tr" s away

groupDeclDoc    :: LA XmlTree XmlTree -> LA XmlTree XmlTree
groupDeclDoc ts   = scanRegexA declDoc ts >>>
        mkelem "tr"
        [ sattr "class" "decl"
        , attr  "id" (unlistA >>> getDeclName >>> mkText)
        ] [unlistA >>> getChildren]
            





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