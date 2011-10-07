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
import W3W.IndexTypes
import Text.JSON
import W3W.Date as D
import Holumbus.Query.Language.Grammar
import Holumbus.Query.Result
import Monad (liftM)
------------------------------------------------------------------------------
-- |
-- | some constants
-- |
------------------------------------------------------------------------------

fhWedelPrefix :: String
fhWedelPrefix = "http://www.fh-wedel.de/"
------------------------------------------------------------------------------
-- | number of hits shown per page
hitsPerPage :: Int
hitsPerPage = 10

maxPages :: Int
maxPages = 3

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

saveHead :: [a] -> a -> a
saveHead [] x = x
saveHead (x:xs) _   = x

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
queryFunction :: Application (Query -> IO (Holumbus.Query.Result.Result PageInfo))
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
    (
      if (cssClass == "")
        then []
        else [(T.pack $ "class", T.pack $ cssClass)]
    )
    xNodes

------------------------------------------------------------------------------
-- | creates a HTML List-Item with css-class-attribute
htmlListItem :: String -> X.Node -> X.Node
htmlListItem cssClass xNode =
  X.Element (T.pack $ "li")
    (
      if (cssClass == "")
        then []
        else [(T.pack $ "class", T.pack $ cssClass)]
    )
    [xNode]

------------------------------------------------------------------------------
-- | creates a HTML List-Item.
-- | Takes the left Date-Context, the Date itself and the right Date-Context
htmlListItemDate :: DateContextType -> String -> String -> String -> String -> X.Node
htmlListItemDate DateInCalender _ leftContext date rightContext =
  htmlLink' "" (fhWedelPrefix ++ leftContext) $
    X.Element (T.pack $ "li")
      [(T.pack $ "class", T.pack $ "calenderDateTeaserText")]
      [ X.Element (T.pack $ "div")
        []
        [htmlSpanTextNode "date" date
        ,htmlSpanTextNode "dateContext" (": " ++ rightContext)
        ]
      ]

htmlListItemDate DateInStdContent linkUrl leftContext date rightContext =
  htmlLink' "" (fhWedelPrefix ++ leftContext) $
    X.Element (T.pack $ "li")
      []
      [ X.Element (T.pack $ "div")
        []
        [htmlSpanTextNode "dateContext" (leftContext ++ " ")
        ,htmlSpanTextNode "date" date
        ,htmlSpanTextNode "dateContext" (" " ++ rightContext)
        ]
      ]

 ------------------------------------------------------------------------------
-- | creates a HTML Txt Node
htmlTextNode :: String -> X.Node
htmlTextNode text = X.TextNode $ T.pack $ text

------------------------------------------------------------------------------
-- | creates a HTML Txt Node in a <span class="???"></span> element
htmlSpanTextNode :: String -> String -> X.Node
htmlSpanTextNode cssClass text =
  X.Element (T.pack $ "span")
    (
      if (cssClass == "")
        then []
        else [(T.pack $ "class", T.pack $ cssClass)]
    )
    [htmlTextNode text]

------------------------------------------------------------------------------
-- | creates a HTML Info Node

htmlLink :: String -> String -> String -> X.Node
htmlLink cssClass href text =
  X.Element (T.pack $ "a")
    (
      [(T.pack $ "href", T.pack $ href)]
      ++
      (
        if (cssClass == "")
          then []
          else [(T.pack $ "class", T.pack $ cssClass)]
      )
    )
    [htmlTextNode text]

------------------------------------------------------------------------------
-- | creates a HTML Info Node

htmlLink' :: String -> String -> X.Node -> X.Node
htmlLink' cssClass href xNode =
  X.Element (T.pack $ "a")
    (
      [(T.pack $ "href", T.pack $ href)]
      ++
      (
        if (cssClass == "")
          then []
          else [(T.pack $ "class", T.pack $ cssClass)]
      )
    )
    [xNode]


data DateContextType = DateInStdContent | DateInCalender
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
docHitToListItem isDate docHit =
  htmlListItem "searchResult" $ subList
  where
    subList = htmlList "" subListItems
    subListItems = [htmlLink' "" (srUri docHit) $ htmlListItem "searchResultTitle" $ htmlTextNode . srTitle $ docHit]
                ++ [htmlLink' "" (srUri docHit) $ htmlListItem "searchResultModified" $ htmlTextNode $ (author . srPageInfo $ docHit)
                  ++ " (" ++ ( modified . srPageInfo $ docHit) ++ ")"]
                ++  if (isDate)
                    then mkDateContexts (srUri docHit) stringOfDateContexts listOfMatchedPositionsDate DateInStdContent
                          (show $ M.toList $ fromMaybe M.empty dateContextMap) -- for debugging only!
                         ++
                         mkDateContexts (srUri docHit) stringOfCalenderContexts listOfMatchedPositionsCalender DateInCalender
                         "" -- fordebugging only!
                    else [htmlLink' "" (srUri docHit) mkContentContext]
                ++ [htmlListItem "score" $ htmlTextNode . show . srScore $ docHit]
    mkContentContext =  htmlListItem "teaserText" $ htmlTextNode teaserText
    teaserText = (++ "...") . L.unwords . L.take numTeaserWords . L.words . contentContext . srPageInfo $ docHit
    stringOfDateContexts = datesContext . srPageInfo $ docHit
    stringOfCalenderContexts = calenderContext . srPageInfo $ docHit
    listOfMatchedPositionsDate     = listOfMaps2listOfPositions . M.toList $ fromMaybe M.empty dateContextMap
    listOfMatchedPositionsCalender = listOfMaps2listOfPositions . M.toList $ fromMaybe M.empty calenderContextMap
    dateContextMap     = M.lookup "dates"    $ srContextMap docHit
    calenderContextMap = M.lookup "calender" $ srContextMap docHit
    listOfMaps2listOfPositions [] = []
    listOfMaps2listOfPositions x = L.map (L.head . IS.toList . snd) x

------------------------------------------------------------------------------
-- | convert the contexts of a date to html-list-items
-- | i.e. given a JSON-String of Date Contexts (date1,date2,date3,date4,...)
-- | and a listOfMatchedPositions = [0,2]
-- | the result will be
-- | <li class="dates"><a href="linkUrl">...date1...</a></li>
-- | ...
-- | or, if it's a date-context:
-- | <li class="dates"><a href="link-to-calender-event">...date3...</a></li>
-- | ...
mkDateContexts :: String -> String -> [Int] -> DateContextType -> String -> [X.Node]
mkDateContexts _ _ [] _ _ = []
mkDateContexts _ "" _ _ _ = []
mkDateContexts linkUrl stringOfDateContexts listOfMatchedPositions dct debugInfo =
  (P.map str2htmlListItem listOfMatchedContexts)
    where
      str2htmlListItem (leftContext,theDate,rightContext) = htmlListItemDate dct linkUrl leftContext theDate rightContext
      listOfMatchedContexts = P.map getDateContextAt listOfMatchedPositions
      getDateContextAt position =
        if (position') > ((L.length listOfDateContexts) - 1)
           then ( "", "bad index: " ++ (show position') ++ " in: " ++ (show listOfDateContexts) ++ " where list is: <" ++ (L.unwords $ P.map show listOfMatchedPositions) ++ ">", "")
            --     else ("contexts: ", (show stringOfDateContexts),
            --        " where list is: <" ++ (L.unwords $ P.map show listOfMatchedPositions) ++ ">"
            --              ++ " and dateContextMap is <" ++ debugInfo ++ ">")
           else showContexts dct 
        where
          showContexts DateInStdContent = ("..." ++ (contexts !! 0), (contexts !! 1), (contexts !! 2) ++ "...")
          showContexts DateInCalender   = ((contexts !! 0), (contexts !! 1), (contexts !! 2) ++ "...")
          position' = position - 1
          contexts = listOfDateContexts !! position'
      listOfDateContexts = fromJson ((decodeStrict stringOfDateContexts) :: Text.JSON.Result [[String]])
      fromJson (Ok a) = a
      fromJson (Error s) = [[s]]

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
mkPagerLink :: String -> Int -> Int -> X.Node
mkPagerLink query actPage number =
  htmlLink cssClass
    ("/querypage?query=" ++ query ++ "&page=" ++ pageNum)
    (" " ++ pageNum ++ " ")
  where
    pageNum = show number
    cssClass = if actPage == number
                  then "actPage"
                  else ""

------------------------------------------------------------------------------
-- | maybe transform the search-query into a normalized date string.
-- | Result: (tranformedStringOrOriginalString, whetherOrNotTheStringIsADate)
maybeNormalizeQuery :: String -> (String, Bool)
maybeNormalizeQuery query =
  (either id id normalizedDateOrQuery, isDate)
  where
    normalizedDates = getNormFunc D.dateRep2NormalizedDates . D.extractDateRep $ query
    isDate = not $ L.null normalizedDates
    normalizedDateOrQuery  = if not isDate
                             then Left query
                             else Right $ L.head normalizedDates

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
  let dateRep = extractDateRep query
  (transformedQuery, numOfTransforms) <-liftIO $ dateRep2stringWithTransformedDates dateRep
  let hasDate = (numOfTransforms > 0)
  liftIO $ P.putStrLn $ transformedQuery -- print debug info to console
  queryFunc' <- queryFunction
  searchResultDocs <- liftIO $ getIndexSearchResults transformedQuery queryFunc'
  strPage <- getQueryStringParam "page"
  let intPage = strToInt 1 strPage
  let indexSplices = [ ("result", resultSplice hasDate intPage searchResultDocs)
                     , ("oldquery", oldQuerySplice)
                     , ("pager", pagerSplice query intPage searchResultDocs)
                     ]
  heistLocal (bindSplices indexSplices) $ render "frontpage"

-- | generates the HTML node to be inserted into "<result />"
resultSplice :: Bool -> Int -> SearchResultDocs -> Splice Application
resultSplice isDate pageNum searchResultDocs = do
  let items = P.map (docHitToListItem isDate) (L.take hitsPerPage $ L.drop ((pageNum-1)*hitsPerPage) $ srDocHits searchResultDocs)
  let docHits = srDocHits searchResultDocs
--  if P.null $ docHits
--     then liftIO $ P.putStrLn "- keine Ergebnisse -"
--     else liftIO $ P.putStrLn . show . (M.member "datesContext") . srContextMap . L.head $ docHits
  let infos = [docHitsMetaInfo searchResultDocs]
  return $ [htmlList "" (infos ++ items)]

-- | generates the HTML node to be inserted into "<oldquery />"
oldQuerySplice :: Splice Application
oldQuerySplice = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- lift $ decodedParam "query"
  let query' = T.unpack (E.decodeUtf8 query)
  return $ [htmlTextNode query']

-- | generates the HTML node to be inserted into "<pager />"
pagerSplice :: String -> Int -> SearchResultDocs -> Splice Application
pagerSplice query actPage searchResultDocs = do
  let resultCount =  L.length $ srDocHits searchResultDocs
  let numberOfPages = max maxPages (ceiling $ (fromIntegral resultCount) / (fromIntegral hitsPerPage)) -- TODO: Defaulting the following constraint(s) to type `Double' arising from a use of `/' at src/Site.hs:225:63
  return $ L.map (mkPagerLink query actPage) [1..numberOfPages]

------------------------------------------------------------------------------
-- |
-- | completions
-- |
------------------------------------------------------------------------------
-- returns the list of found completions to the Ajax-caller
completions :: Application ()
completions = do
  query'' <- getQueryStringParam "query"
  let (query', isDate) = maybeNormalizeQuery query'' -- determine if its a date
  query <-  if isDate
            then liftIO $ prepareNormDateForCompare (query', "") -- if its a date, truncate trailing "*"s and replace leading "*"s
            else return query'
  queryFunc' <- queryFunction
  searchResultWords' <- liftIO $ getWordCompletions query $ queryFunc'
  let searchResultWords = if isDate
                          then L.map (\ (SRWordHit word hit) -> SRWordHit (D.unNormalizeDate word) hit) $ srWordHits searchResultWords'
                          else srWordHits searchResultWords'
  putResponse myResponse
  writeText (T.pack $ toJSONArray numDisplayedCompletions $ searchResultWords)
  where
  myResponse = setContentType "text/plain; charset=utf-8" . setResponseCode 200 $ emptyResponse

-- convert List to JSON-Array
toJSONArray :: Int -> [SRWordHit] -> String
toJSONArray n srwh = encodeStrict $ showJSONs (P.map (\ (SRWordHit w1 h1) -> w1 ++ " (" ++ (show h1) ++ ")") (L.take n srwh))

------------------------------------------------------------------------------

