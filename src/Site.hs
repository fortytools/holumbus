{-# LANGUAGE OverloadedStrings #-}

{-

This is where all the routes and handlers are defined for our site. The
'site' function combines everything together and is exported by this module.

-}

module Site ( site ) 

where

import Control.Applicative
import Data.Maybe
import List as L
import Data.Text as T
import Data.Text.Encoding as E
import Snap.Extension.Heist
import Snap.Util.FileServe
import Snap.Types
import Text.Templating.Heist
import Application
import W3WState
import EvalSearch
import Control.Monad.Trans
import W3WSimpleSearch
import qualified  Text.XmlHtml as X
import Prelude as P
import Data.Map as M
import Data.IntSet as IS
--import Text.Regex (splitRegex, mkRegex)
import IndexTypes
import Text.JSON hiding (Result)
import W3W.Date as D
import Holumbus.Query.Language.Grammar
import Holumbus.Query.Result
import Data.Time.Clock
import Data.Time.Calendar

------------------------------------------------------------------------------
-- |
-- | some constants
-- |
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | number of hits shown per page
hitsPerPage :: Int
hitsPerPage = 5

------------------------------------------------------------------------------
-- | number of words contained in the teaser text
numTeaserWords :: Int
numTeaserWords = 30

------------------------------------------------------------------------------
-- | number of word completions send in response to the Ajax request
numDisplayedCompletions :: Int
numDisplayedCompletions = 20

------------------------------------------------------------------------------
-- |
-- | some little helpers
-- |
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | convert a ByteString to an Int.
-- | returns defaultValue if conversion fails
strToInt :: Int -> String -> Int
strToInt defaultValue str
  | (L.length readsResult > 0) = fst $ L.head readsResult
  | otherwise = defaultValue
  where 
  readsResult = reads $ str

------------------------------------------------------------------------------
-- | get the Index-Data
getCoreIdx :: Application CompactInverted
getCoreIdx = do
  core <- w3wCore
  return $ EvalSearch.index core

------------------------------------------------------------------------------
-- | get the Document-Data
getCoreDoc :: Application (SmallDocuments PageInfo)
getCoreDoc = do
  core <- w3wCore
  return $ EvalSearch.documents core

------------------------------------------------------------------------------
-- | the function that does the query
queryFunction :: Application (Query -> IO (Result PageInfo))
queryFunction = do
  doc <- getCoreDoc
  idx <- getCoreIdx
  return $ localQuery idx doc

------------------------------------------------------------------------------
-- | get the value associated to a specific param from the Query-String
getQueryStringParam :: String -> Application String
getQueryStringParam param = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- decodedParam $ encodeUtf8 $ T.pack param
  return $ T.unpack (E.decodeUtf8 query)

------------------------------------------------------------------------------
-- | creates a HTML List-Item with css-class-attribute
htmlList :: String -> [X.Node] -> X.Node
htmlList cssClass xNodes = 
  X.Element (T.pack $ "ul") 
    [(T.pack $ "class", T.pack $ cssClass)] 
    xNodes
  
------------------------------------------------------------------------------
-- | creates a HTML List-Item with css-class-attribute
htmlListItem :: String -> X.Node -> X.Node
htmlListItem cssClass xNode = 
  X.Element (T.pack $ "li") 
    [(T.pack $ "class", T.pack $ cssClass)] 
    [xNode]
  
------------------------------------------------------------------------------
-- | creates a HTML Txt Node
htmlTextNode :: String -> X.Node
htmlTextNode text = X.TextNode $ T.pack $ text

------------------------------------------------------------------------------
-- | creates a HTML Info Node

htmlLink :: String -> String -> String -> X.Node
htmlLink cssClass href text = 
  X.Element (T.pack $ "a") 
    [ (T.pack $ "href", T.pack $ href),
      (T.pack $ "class", T.pack $ cssClass)
    ]
    [htmlTextNode text]

------------------------------------------------------------------------------
-- | creates a HTML Info Node

htmlLink' :: String -> String -> X.Node -> X.Node
htmlLink' cssClass href xNode = 
  X.Element (T.pack $ "a") 
    [ (T.pack $ "href", T.pack $ href),
      (T.pack $ "class", T.pack $ cssClass)
    ]
    [xNode]

------------------------------------------------------------------------------
-- | creates a HTML List-Item containing a List with the link to the document found, the teasertext and the ranking-score
-- | i.e.
-- | <li>
-- |   <ul>
-- |     <li><a href="linkToDocumentFound">titleOfDocumentFound</a></li>
-- |     <li>teaserText</li>
-- |     <li>rankingScoreOfDocumetFound</li>
-- |   </ul>
-- | </li>
docHitToListItem :: Bool -> SRDocHit -> X.Node
docHitToListItem isDate docHit = htmlListItem "searchResult_li" $ 
  htmlLink' "ul" (srUri docHit) $ subList
  where
    subList = htmlList "searchResult_ul" subListItems
    subListItems = [htmlListItem "link" $ htmlTextNode . srTitle $ docHit]
                ++ [htmlListItem "author_modified" $ htmlTextNode $ (author . srPageInfo $ docHit) 
                  ++ " (" ++ ( modified . srPageInfo $ docHit) ++ ")"]
                ++  if (isDate) 
                    then dateContexts stringOfDateContexts listOfMatchedPositions 
                    else contentContext
--              ++ [htmlListItem "debug" $ htmlTextNode $ " <isDate: " ++ (show isDate) ++ "> "]
--              ++ [htmlListItem "debug" $ htmlTextNode $ " <listOfMatchedPositions: " ++ (show listOfMatchedPositions) ++ "> "]
                ++ [htmlListItem "score" $ htmlTextNode . show . srScore $ docHit]  
    contentContext =  [htmlListItem "content" $ htmlTextNode teaserText]
    teaserText = (++ "...") . L.unwords . L.take numTeaserWords . L.words . content . srPageInfo $ docHit
    stringOfDateContexts = dates . srPageInfo $ docHit
    listOfMatchedPositions = listOfMaps2listOfPositions . M.toList $ fromMaybe M.empty dateContextMap
    dateContextMap = M.lookup "dates" $ srContextMap docHit
    listOfMaps2listOfPositions [] = []
    listOfMaps2listOfPositions l = IS.toList . snd . L.head $ l
 
------------------------------------------------------------------------------
-- | convert the contexts of a date to html-list-items
-- | i.e. given a JSON-String of Date Contexts (date1,date2,date3,date4,...)
-- | and a listOfMatchedPositions = [0,2]
-- | the result will be 
-- | <li class="dates">...date1...</li>
-- | <li class="dates">...date3...</li>
dateContexts :: String -> [Int] -> [X.Node]
dateContexts _ [] = []
dateContexts "" _ = []
dateContexts stringOfDateContexts listOfMatchedPositions = 
  (P.map str2htmlListItem listOfMatchedContexts) 
--  ++ [htmlListOfDateContexts] -- TODO: einkommentieren zum Debuggen, damit alle date-Kontexte einer Seite angezigt werden
--  ++ [htmlListItem "matched_positions" $ htmlTextNode $ L.unwords $ P.map show listOfMatchedPositions] -- TODO: einkommentieren, damit die Posiionen der Fundstellen angezeigt werden
    where
      str2htmlListItem dateContext = htmlListItem "dates" $ htmlTextNode dateContext
      listOfMatchedContexts = P.map getDateContextAt listOfMatchedPositions  
      getDateContextAt position = 
        if (position') > ((L.length listOfDateContexts) - 1)
        then "bad index: " ++ (show position') ++ " in: " ++ stringOfDateContexts ++ " where list is: <" ++ (L.unwords $ P.map show listOfMatchedPositions) ++ ">"
        else "..." ++ (listOfDateContexts !! position') ++ "..."
          where
          position' = position - 1
      listOfDateContexts = fromJson $ decodeStrict stringOfDateContexts
      fromJson (Ok a) = a
      fromJson (Error s) = [s]
      htmlListOfDateContexts = htmlList "date_contexts" $ P.map str2htmlListItem listOfDateContexts

------------------------------------------------------------------------------
-- | creates the HTML info text describing the search result (i.e. "Found 38 docs in 0.0 sec.")
docHitsMetaInfo :: SearchResultDocs -> X.Node
docHitsMetaInfo searchResultDocs = 
  htmlListItem "info" $ htmlTextNode $ 
    "Found " ++ 
    (show $ srDocCount searchResultDocs) ++ 
    " docs in " ++ 
    (show $ srTime searchResultDocs) ++ " sec."

------------------------------------------------------------------------------
-- | creates a HTML Link used in the Pager Splice
-- |   query: the search-query
-- |   number: Number displayed in the Pager-Link (the text node of the link)
-- |   takeHits: Number of Hits to be displayed per Site
-- |   dropHits: Number of Hits to be dropped from the Result of all Document-Hits
-- | i.e. for Pager-Link No. 4, searching for "Wedel": <a href="/querypage?query=Wedel&takeHits=10&dropHits=30"> 4 </a>
mkPagerLink :: String -> (Int, Int, Int) -> X.Node
mkPagerLink query (number, takeHits, dropHits) = 
  htmlLink "pager"
    ("/querypage?query=" ++ query ++ "&takeHits=" ++ (show takeHits) ++ "&dropHits=" ++ (show dropHits))
    (" " ++ (show number) ++ " ")

------------------------------------------------------------------------------
-- | maybe transform the search-query into a normalized date string.
-- | Result: (tranformedStringOrOriginalString, whetherOrNotTheStringIsADate)
maybeNormalizeQuery :: String -> (String, Bool)
maybeNormalizeQuery query = 
  (either id id normalizedDateOrQuery, isDate)
  where
    normalizedDates = D.dateRep2NormalizedDates . D.extractDateRep $ query
    isDate = not $ L.null normalizedDates
    normalizedDateOrQuery  = if not isDate
                             then Left query
                             else Right $ L.head normalizedDates

------------------------------------------------------------------------------
-- | prepare a normalized Date-String (i.e. "****-**-03-12-**") for comparison with indexed normalized dates.
-- | the leading "****-**-" will be replaced with the actual date.
-- | since the comparison will be prefix-based the trailing "-**" can be truncated.
prepareNormDateForCompare :: String -> IO String
prepareNormDateForCompare normDate = do
  s <- fillNormDate normDate
  return $ truncNormDate s
  where
    truncNormDate = L.reverse . truncNormDate' . L.reverse
    truncNormDate' normDate@(x:xs)
      | (x == '*' || x == '-') = truncNormDate' xs
      | otherwise = normDate
    fillNormDate d = do
      curr <- currentTimeStr
      return $ fillNormDate' d curr
    fillNormDate' _ [] = []
    fillNormDate' normDate@(x:xs) (y:ys)
      | (x == '*' || x == '-') = y:(fillNormDate' xs ys)
      | otherwise = normDate
    currentTimeStr = do
      utcTime <- getCurrentTime
      let day = utctDay utcTime
      return (showGregorian day)

------------------------------------------------------------------------------
-- |
-- | end of helpers
-- |
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
-- | site
-- |
------------------------------------------------------------------------------
-- | defines routes through our site
site :: Application ()
site = route [
              ("/",          frontpage),
              ("/querypage", processquery), -- render HTML Page with found Document-Hits
              ("/completions", completions) -- return List of Completions to Ajax-caller
             ]
  <|> serveDirectory "resources/static"

------------------------------------------------------------------------------
-- |
-- | frontpage
-- |
------------------------------------------------------------------------------
-- | renders the front page.
-- | simply display "frontpage.tpl" Template-file without substituting any <... /> Tags
frontpage :: Application ()
frontpage = ifTop $ render "frontpage"


------------------------------------------------------------------------------
-- |
-- | processquery
-- |
------------------------------------------------------------------------------
-- | render HTML Page with the doc-hits found.
-- | display "frontpage.tpl" Template-file with substituting following Tags:
-- |   <result />
-- |   <oldquery />
-- |   <pager />
processquery :: Application ()
processquery = do
  query <- getQueryStringParam "query"
  let (query', isDate) = maybeNormalizeQuery query
  query'' <-  if isDate
              then liftIO $ prepareNormDateForCompare query'
              else return query'
  liftIO $ P.putStrLn query'' -- print debug info to console
  queryFunc' <- queryFunction
  searchResultDocs <- liftIO $ getIndexSearchResults query'' queryFunc'
  strTakeHits <- getQueryStringParam "takeHits"
  strDropHits <- getQueryStringParam "dropHits"
  let intDropHits = strToInt 0 strDropHits
  let intTakeHits = strToInt hitsPerPage strTakeHits
  let indexSplices = [ ("result", resultSplice isDate intTakeHits intDropHits searchResultDocs)
                     , ("oldquery", oldQuerySplice)
                     , ("pager", pagerSplice query searchResultDocs)
                     ]
  heistLocal (bindSplices indexSplices) $ render "frontpage"

-- | generates the HTML node to be inserted into "<result />"
resultSplice :: Bool -> Int -> Int -> SearchResultDocs -> Splice Application
resultSplice isDate takeHits dropHits searchResultDocs = do
  let items = P.map (docHitToListItem isDate) (L.take takeHits $ L.drop dropHits $ srDocHits searchResultDocs)
--  liftIO $ P.putStrLn . show . (member "dates") . srContextMap . L.head . srDocHits $ searchResultDocs
  let infos = [docHitsMetaInfo searchResultDocs]
  return $ [htmlList "searchResultList" (infos ++ items)]

-- | generates the HTML node to be inserted into "<oldquery />"
oldQuerySplice :: Splice Application
oldQuerySplice = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- lift $ decodedParam "query"
  let query' = T.unpack (E.decodeUtf8 query)
  return $ [htmlTextNode query']

-- | generates the HTML node to be inserted into "<pager />"
pagerSplice :: String -> SearchResultDocs -> Splice Application
pagerSplice query searchResultDocs = do
  let resultCount =  L.length $ srDocHits searchResultDocs
  let maxNumberOfPages = ceiling $ (fromIntegral resultCount) / (fromIntegral hitsPerPage) -- TODO: Defaulting the following constraint(s) to type `Double' arising from a use of `/' at src/Site.hs:225:63
  let mkLinkTriple hitsPerPage currentPage = (currentPage, hitsPerPage, (currentPage-1)*hitsPerPage) -- TODO: binding for `hitsPerPage' shadows the existing binding defined at src/Site.hs:48:1
  let linkTriples = L.map (mkLinkTriple hitsPerPage) [1..maxNumberOfPages]
  return $ L.map (mkPagerLink query) linkTriples

------------------------------------------------------------------------------
-- |
-- | completions
-- |
------------------------------------------------------------------------------
-- returns the list of found completions to the Ajax-caller
completions :: Application ()
completions = do
  query <- getQueryStringParam "query"
  queryFunc' <- queryFunction
  searchResultWords <- liftIO $ getWordCompletions query $ queryFunc'
  putResponse myResponse
  writeText (T.pack $ toJSONArray numDisplayedCompletions $ srWordHits searchResultWords)
  where
  myResponse = setContentType "text/plain; charset=utf-8" . setResponseCode 200 $ emptyResponse

-- convert List to JSON-Array
toJSONArray :: Int -> [SRWordHit] -> String
toJSONArray n srwh = encodeStrict $ showJSONs (P.map (\ (SRWordHit w1 h1) -> w1 ++ " (" ++ (show h1) ++ ")") (L.take n srwh))

------------------------------------------------------------------------------

