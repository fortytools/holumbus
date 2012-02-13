-- ----------------------------------------------------------------------------

{- |
  Module     : Helpers

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable

  Some helpers that don't fit into other modules
-}

-- ----------------------------------------------------------------------------

module Helpers
where

import Data.List                as L
import qualified Data.Text      as T

import Text.JSON
import qualified Text.XmlHtml   as X

-- ----------------------------------------------------------------------------

fhWedelPrefix :: String
fhWedelPrefix = "http://www.fh-wedel.de/"

-- ----------------------------------------------------------------------------
-- | Make an one-item-List      

box :: a -> [a]
box x = [x]

-- ------------------------------------------------------------------------------
-- | convert a String to an Int.
-- | returns defaultValue if conversion fails

strToInt :: Int -> String -> Int
strToInt defaultValue str
  | (length readsResult > 0) = fst $ head readsResult
  | otherwise = defaultValue
  where
  readsResult = reads $ str


saveHead :: [a] -> a -> a
saveHead [] x = x
saveHead (x:_) _   = x

-- ------------------------------------------------------------------------------
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

-- ------------------------------------------------------------------------------
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

data DateContextType = DateInStdContent | DateInCalender

-- ------------------------------------------------------------------------------
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
  htmlLink' "" (linkUrl) $
    X.Element (T.pack $ "li")
      []
      [ X.Element (T.pack $ "div")
        []
        [htmlSpanTextNode "dateContext" (leftContext ++ " ")
        ,htmlSpanTextNode "date" date
        ,htmlSpanTextNode "dateContext" (" " ++ rightContext)
        ]
      ]

 -- ------------------------------------------------------------------------------
-- | creates a HTML Txt Node

htmlTextNode :: String -> X.Node
htmlTextNode text = X.TextNode $ T.pack $ text

-- ------------------------------------------------------------------------------
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

-- ------------------------------------------------------------------------------
-- | creates a HTML Txt Node in a <p></p> element

htmlParaTextNode :: String -> X.Node
htmlParaTextNode text =
  X.Element (T.pack $ "p")
    []
    [htmlTextNode text]

-- ------------------------------------------------------------------------------
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

-- ------------------------------------------------------------------------------
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


-- ------------------------------------------------------------------------------
-- | convert the contexts of a date to html-list-items
-- | i.e. given a JSON-String of Date Contexts (date1,date2,date3,date4,...)
-- | and a listOfMatchedPositions = [0,2]
-- | the result will be
-- | <li class="dates"><a href="linkUrl">...date1...</a></li>
-- | ...
-- | or, if it's a date-context:
-- | <li class="dates"><a href="link-to-calender-event">...date3...</a></li>
-- | ...

mkDateContexts :: (Show i, Enum i) => String -> String -> [i] -> DateContextType -> String -> [X.Node]
mkDateContexts _ _ [] _ _ = []
mkDateContexts _ "" _ _ _ = []
mkDateContexts linkUrl stringOfDateContexts listOfMatchedPositions dct _ =
  (map str2htmlListItem listOfMatchedContexts)
    where
      str2htmlListItem (leftContext,theDate,rightContext) = htmlListItemDate dct linkUrl leftContext theDate rightContext
      listOfMatchedContexts = map (getDateContextAt . fromEnum) listOfMatchedPositions
      getDateContextAt position =
        if (position') > ((L.length listOfDateContexts) - 1)
          then ( "", "bad index: " ++ (show position') ++ " in: " ++ (show listOfDateContexts) ++ " where list is: <" ++ (L.unwords $ map show listOfMatchedPositions) ++ ">", "")
          -- else ("contexts: ", (show stringOfDateContexts),
          --      " where list is: <" ++ (L.unwords $ map show listOfMatchedPositions) ++ ">"
          -- last param must be "debugInfo to make this work"
          --      ++ " and dateContextMap is <" ++ debugInfo ++ ">")
          else showContexts dct
        where
          showContexts DateInStdContent = ("..." ++ (contexts !! 0), (contexts !! 1), (contexts !! 2) ++ "...")
          showContexts DateInCalender   = ((contexts !! 0), (contexts !! 1), (contexts !! 2) ++ "...")
          position' = position - 1
          contexts = listOfDateContexts !! position'
      listOfDateContexts = fromJson ((decodeStrict stringOfDateContexts) :: Text.JSON.Result [[String]])
      fromJson (Ok a) = a
      fromJson (Error s) = [[s]]


-- ------------------------------------------------------------------------------
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


-- ------------------------------------------------------------------------------
-- | The help-text rendered when no results are found

examples :: X.Node
examples
    = X.Element (T.pack $ "div")
      [(T.pack $ "class", T.pack $ "examples")]
      [ htmlParaTextNode "Beispiele für Suchanfragen:"
      , htmlList "examples"
        [ htmlListItem "" $
          X.Element (T.pack $ "div")
          [ (T.pack $ "class", T.pack $ "example") ]
          [ htmlLink "" "querypage?query=Holumbus%20AND%20Hayoo&button=Suchen" "Holumbus AND Hayoo"
          , htmlSpanTextNode "" " sucht nach Seiten, die sowohl das Wort "
          , htmlSpanTextNode "green" "Holumbus"
          , htmlSpanTextNode "" " als auch das Wort "
          , htmlSpanTextNode "green" "Hayoo"
          , htmlSpanTextNode "" " enthalten. (Kurzform: "
          , htmlLink "" "querypage?query=Holumbus%20Hayoo&button=Suchen" "Holumbus Hayoo"
          , htmlSpanTextNode "" ")."
          ]
        , htmlListItem "" $
          X.Element (T.pack $ "div")
          [ (T.pack $ "class", T.pack $ "example") ]
          [ htmlLink "" "querypage?query=Holumbus%20OR%20Hayoo&button=Suchen" "Holumbus OR Hayoo"
          , htmlSpanTextNode "" " sucht nach Seiten, die mindestens eines der beider Wörter enthalten."
          ]
        , htmlListItem "" $
          X.Element (T.pack $ "div")
          [ (T.pack $ "class", T.pack $ "example") ]
          [ htmlLink "" "querypage?query=Haskell%20AND%20NOT%20(Holumbus%20OR%20Hayoo)&button=Suchen" "Haskell AND NOT (Holumbus OR Hayoo)"
          , htmlSpanTextNode "" " sucht nach Seiten, die das Wort "
          , htmlSpanTextNode "green" "Haskell"
          , htmlSpanTextNode "" ", aber keines der Wörter "
          , htmlSpanTextNode "green" "Holumbus"
          , htmlSpanTextNode "" " oder "
          , htmlSpanTextNode "green" "Hayoo"
          , htmlSpanTextNode "" " enthalten."
          ]
        , htmlListItem "" $
          X.Element (T.pack $ "div")
          [ (T.pack $ "class", T.pack $ "example") ]
          [ htmlLink "" "querypage?query=Seminar%20Mai%202011&button=Suchen" "Seminar Mai 2011"
          , htmlSpanTextNode "" " sucht nach allen Vorkommen des Textes "
          , htmlSpanTextNode "green" "Seminar"
          , htmlSpanTextNode "" " und einem Datum des Monats Juni im Jahr 2011. Gefunden werden Seiten, die Daten wie "
          , htmlSpanTextNode "green" "1.5.'11"
          , htmlSpanTextNode "" " oder "
          , htmlSpanTextNode "green" "5. Mai"
          , htmlSpanTextNode "" " (bei Daten ohne Jahresangabe wird das aktuelle Jahr angenommen) enthalten. Genauso funktionieren viele weitere gängige Datumsformate wie z.B.: "
          , htmlLink "" "querypage?query=21.%20September%20um%2013%20Uhr&button=Suchen" "21. September um 13 Uhr"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=3.6.%2011:00&button=Suchen" "13.6. 9:00"
          ]
        , htmlListItem "" $
          X.Element (T.pack $ "div")
          [ (T.pack $ "class", T.pack $ "example") ]
          [ htmlLink "" "querypage?query=PTL%20dieser%20Monat&button=Suchen" "PTL dieser Monat"
          , htmlSpanTextNode "" " sucht nach allen Vorkommen des Textes "
          , htmlSpanTextNode "green" "PTL"
          , htmlSpanTextNode "" " und einem Datum des aktuellen Monats, sowie nach dem Text "
          , htmlSpanTextNode "green" "dieser Monat"
          , htmlSpanTextNode "" ". Genauso funktionieren folgende weitere Abkürzungen zum Suchen nach Daten: "
          , htmlLink "" "querypage?query=heute&button=Suchen" "heute"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=diese%20Woche&button=Suchen" "diese Woche"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=nächste%20Woche&button=Suchen" "nächste Woche"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=dieser%20Monat&button=Suchen" "dieser Monat"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=nächster%20Monat&button=Suchen" "nächster Monat"
          {-
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=übernächster%20Monat&button=Suchen" "übernächster Monat"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=dieses%20Jahr&button=Suchen" "dieses Jahr"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=nächstes%20Jahr&button=Suchen" "nächstes Jahr"
          -- -}
          , htmlSpanTextNode "" "."
          , htmlSpanTextNode "" "Die Eingaben der Terminsuche können abgekürzt werden: "
          , htmlLink "" "querypage?query=di%20wo&button=Suchen" "di wo"
          , htmlSpanTextNode "" " oder "
          , htmlLink "" "querypage?query=diwo&button=Suchen" "diwo"
          , htmlSpanTextNode "" "werden wie "
          , htmlLink "" "querypage?query=diese%20Woche&button=Suchen" "diese Woche"
          , htmlSpanTextNode "" " interpretiert."
          , htmlSpanTextNode "" "Termine für bestimmte Monate können ebenfalls abgekürzt gesucht werden. Bei einer Eingabe von "
          , htmlLink "" "querypage?query=September&button=Suchen" "September"
          , htmlSpanTextNode "" " oder "
          , htmlLink "" "querypage?query=sep&button=Suchen" "sep"
          , htmlSpanTextNode "" " werden Termine im kommenden (oder laufenden) September gesucht."
          ]
        , htmlListItem "" $
          X.Element (T.pack $ "div")
          [ (T.pack $ "class", T.pack $ "example") ]
          [ htmlLink "" "querypage?query=Kalender%20dieser Monat&button=Suchen" "Kalender dieser Monat"
          , htmlSpanTextNode "" " findet unter anderem die gesuchten Daten im FH Wedel Kalender. Die dort aufgelisteten Daten können direkt angeklickt werden um zu den entsprechenden Unterseiten zu gelangen."
          ]
        ]
      ]

-- ------------------------------------------------------------------------------
