{-# OPTIONS #-}

-- ------------------------------------------------------------

module Examples2.Re.Common
where

import Data.Function.Selector
import Data.Char
import Data.Maybe
import Data.List
import Holumbus.DCrawler.Core
import Holumbus.DCrawler.Html
import Holumbus.DCrawler.Util
import Holumbus.DCrawler.IndexerCore
import Holumbus.DCrawler.URIs
import Holumbus.DCrawler.Constants

import Text.XML.HXT.Arrow

--
import Control.Parallel.Strategies
import Data.Binary      ( Binary )
import qualified Data.Binary      as B      -- else naming conflict with put and get from Monad.State

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted.Memory
import Holumbus.Index.Documents
import Holumbus.Index.Common hiding (URI) -- (DocId,HolIndex,HolDocuments)

import qualified Data.Map as M

-- ------------------------------------------------------------------------------------------------------------------------------------
type Result = (ResultIndex, ResultState)
type ResultState = (CrawlerState PlainDocs, M.Map URI DocId)
type ResultIndex = IndexerState Inverted Documents PlainText
type PlainDoc = RawDoc PlainText
type PlainDocs = [(URI, PlainDoc)]
type TextCrawlerConfig  = CrawlerConfig PlainDoc PlainDocs

newtype PlainText   = PT { unPT :: String } deriving (Show)

instance Binary PlainText where
    get       = B.get >>= (return . PT)
    put       = B.put . unPT

instance NFData PlainText where
    rnf       = rnf . unPT

instance XmlPickler PlainText where
    xpickle       = xpElem "text" $
          xpWrap (PT , unPT)
          xpText0
-- ------------------------------------------------------------------------------------------------------------------------------------


doc :: Int
doc = 200

crawlerInitState  :: ResultState
crawlerInitState  = (initCrawlerState [], M.empty)

emptyIndex :: ResultIndex
emptyIndex = emptyIndexerState emptyInverted emptyDocuments

emptyState :: ResultState
emptyState = crawlerInitState

crawlerConfig	:: String -> Int -> TextCrawlerConfig
crawlerConfig	follow num = addReadAttributes  defaultOpts				-- at the moment no more read attributes are neccessary
  >>> setS theProcessRefs   ( getHtmlReferences  )
  >>> setS thePreDocFilter	documentOK
  >>> setS theProcessDoc	theDocs
  >>> setS theFollowRef ( simpleFollowRef' [follow] [] )
  >>> setS theMaxNoOfDocs num
  >>> setS theSaveIntervall 10                                 -- every 10 documents the state is saved
  >>> setS theSavePathPrefix "./tmp/re-"                       -- states are saved in subdir "./tmp" in files starting with "re-"
  >>> setS theTraceLevel 1                                     -- trace actions with lowest level
  $ defaultHtmlCrawlerConfig accuHeading
  where
    accuHeading :: AccumulateDocResult PlainDoc PlainDocs
    accuHeading urianddoc =  return . (:) urianddoc
  
    documentOK = ( getAttrValue transferStatus >>> isA (== "200") ) `guards`	this-- document transfer status must be 200 OK
  
    theDocs	:: IOSArrow XmlTree PlainDoc
    theDocs = ( listA contextFs &&& titleF &&& customF ) >>^ (\ (x3, (x2, x1)) -> (x3, x2, x1))
      where
      titleF = getHtmlTitle
      
      customF = getPlainText128 >. listToMaybe
            
      contextFs :: IOSArrow XmlTree RawContext
      contextFs = catA . map contextF $ ixcs
                     
      contextF :: IndexContextConfig -> IOSArrow XmlTree RawContext
      contextF ixc = constA (ixc_name ixc) &&& ( ixc_collectText ixc >. processText ) -- the name of the raw context -- the list of words and positions of the collected text
        where
          processText   :: [String] -> RawWords
          processText   = concat >>> ixc_textToWords ixc >>> flip zip [1..] >>> filter (fst >>> ixc_boringWord ixc >>> not)
          
    defaultOpts     = [ (curl_max_filesize,   "1000000")  -- limit document size to 1 Mbyte
        , (curl_location,     v_1)    -- automatically follow redirects
        , (curl_max_redirects,  "3")    -- but limit # of redirects to 3
        , (a_accept_mimetypes,  "text/html")
        , (a_encoding,    isoLatin1)
        , (a_ignore_encoding_errors,  v_1)      -- encoding errors and parser warnings are boring
        , (a_validate,      v_0)
        , (a_parse_html,    v_1)
        , (a_issue_warnings,  v_0)
      ]
  
-- ------------------------------------------------------------

ixDefault :: IndexContextConfig
ixDefault = IndexContextConfig {
    ixc_name   = "default"
  , ixc_collectText  = getHtmlPlainText
  , ixc_textToWords  = deleteNotAllowedChars >>> words
  , ixc_boringWord = boringWord
}

ixcs :: [ IndexContextConfig ]
ixcs = [ ixDefault
    { ixc_name    = "title"
    , ixc_collectText = getHtmlTitle
    }
  , ixDefault
    { ixc_name    = "headlines"
    , ixc_collectText = getAllText getHeadlines
    }
  , ixDefault
    { ixc_name    = "content"
    , ixc_collectText = getAllText getDivCol2
    }]

getBody :: ArrowXml a => a XmlTree XmlTree
getBody = this />  hasName "html" />  hasName "body"

getHeadlines :: ArrowXml a => a XmlTree XmlTree
getHeadlines = getBody //> hasNameWith (localPart >>> (`elem` ["h1","h2","h3","h4","h5","h6"]))

getDivCol2 :: ArrowXml a => a XmlTree XmlTree
getDivCol2 = getBody //> ( hasName "div" >>> hasAttrValue "id" (== "col2_content")) -- contents part of fh layout

getPlainText128   :: ArrowXml a => a XmlTree PlainText
getPlainText128   = getAllText getBody
        >>^
        (normalizeWS >>> limitLength 128 >>> PT)

-- ------------------------------------------------------------

boringWord :: String -> Bool
boringWord w = length w <= 1
  || all isXmlDigit w

isAllowedWordChar :: Char -> Bool
isAllowedWordChar c = isXmlLetter c
  || isXmlDigit c
  || c `elem` "_-"

deleteNotAllowedChars :: String -> String
deleteNotAllowedChars = map notAllowedToSpace
  where
  notAllowedToSpace c
    | isAllowedWordChar c = c
    | otherwise   = ' '

{-
merges two states. Strategies used to merge a field
  cs_toBeProcessed    :: ! URIs   --> union of sets
  cs_alreadyProcessed :: ! URIs   --> union of sets
  cs_robots           :: ! Robots --> union of maps (what to do with duplicates?)
  cs_noOfDocs         :: ! Int    --> set to -1, means unlimited
  cs_resultAccu       :: ! r      --> needs result merge function
-}
mergeStates :: [ResultState] -> ResultState
mergeStates []           = error "No State"
mergeStates (first':[])   = first'
mergeStates (first':rest') = foldl' mergeStates' first' rest'

mergeStates' :: ResultState -> ResultState -> ResultState
mergeStates' (first',map') (second',_) = (CrawlerState {
    cs_toBeProcessed    = unionURIs a a'
  , cs_newURIs          = unionURIs f f'
  , cs_alreadyProcessed = unionURIs b b'
  , cs_robots   = M.union c c'
  , cs_noOfDocs   = d+d'
  , cs_resultAccu = e ++ e'
  }, map')
  where
    -- get fields from first state
    a  = cs_toBeProcessed first'
    b  = cs_alreadyProcessed first'
    c  = cs_robots first'
    d  = cs_noOfDocs first'
    e  = cs_resultAccu first'
    f  = cs_newURIs first'    
    -- get fields from second state
    a' = cs_toBeProcessed second'
    b' = cs_alreadyProcessed second'
    c' = cs_robots second'
    d' = cs_noOfDocs second'
    e' = cs_resultAccu second'
    f' = cs_newURIs second'
-- -----------------------------------------------------------
{-
  merge list of indexerstates i d c to one
-}
mergeIndices :: [ResultIndex] -> ResultIndex
mergeIndices []           = error "No State"
mergeIndices (first':[])   = first'
mergeIndices (first':rest) = foldl' mergeIndices' first' rest

mergeIndices' :: ResultIndex -> ResultIndex -> ResultIndex
mergeIndices' first' second' = if (null conflicts)
  then IndexerState {
        ixs_index = mergeIndexes idx idx'
      , ixs_documents = docs
    }
  else
    error ("there where conflicts\n+++\n" ++ show conflicts ++ "\n-- idd\n" ++ show idd ++ "\n-- idd'\n" ++ show idd' ++ "\n-- idd'\n" ++ show docs)
  where
    -- get fields from first state
    idx  = ixs_index first'
    idd  = ixs_documents first'
    -- get fields from second state
    idx'  = ixs_index second'
    idd'  = ixs_documents second'
    -- merge docs
    (conflicts, docs) = mergeDocs idd idd'
