-- ----------------------------------------------------------------------------

{- |
  Module     : Extract

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable

  Helper functions and arrows for building the index.
  
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS #-}

module Extract
where
import            Data.Char.Properties.XMLCharProps    ( isXmlLetter
                                                       , isXmlDigit
                                                       )
import            Data.Char                            ( toLower )
import            Data.List                            ( isPrefixOf )

import            Date

import            Holumbus.Crawler

import            Text.XML.HXT.Core

-- ----------------------------------------------------------------------------
-- | Extract text of all h1-h6 tags.

getHeadlines                    :: ArrowXml a => a XmlTree String
getHeadlines                    = fromLA     $
                                  getAllText $
                                  ( getByPath ["html", "body"]
                                    //>
                                    ( (isElem >>> getLocalPart >>> isA isHname)
                                      `guards`
                                      getChildren
                                    )
                                  )
    where
    isHname ('h':d:[])          = d `elem` "123456"
    isHname _                   = False

-- ----------------------------------------------------------------------------
-- | Combine two IOSLA-arrows. The result is the tuple of the two result lists.
-- | Used in getCalenderInfo to combine dates and times placed in different html nodes.

(&&&&)               :: IOSLA t t1 t2 -> IOSLA t t1 t3 -> IOSLA t t1 ([t2], [t3])
IOSLA f &&&& IOSLA g =  IOSLA $ \ s x -> do
                                         (s1, ys1) <- f s  x
                                         (s2, ys2) <- g s1 x
                                         return ( s2, [ ( [y1 | y1 <- ys1], [y2 | y2 <- ys2] ) ] )

-- ----------------------------------------------------------------------------
-- | Data-Type describing the information found on "http://www.fh-wedel.de/online-campus/termine/".
-- | A List of dates and a List of times is associated to a url of a link and the text of the link:
-- | (([date], [time]), (href, text))

type CalenderInfo = (([String], [String]), (String, String))


-- ----------------------------------------------------------------------------
-- | If the site "http://www.fh-wedel.de/online-campus/termine/" is found,
-- | read the info and fill the CalenderInfo data-type.
-- |
-- | Example: (The following html describes an event beginning at 01.12.2011 12:00 and ending at 06.01.2012 23:59)
-- | <div class="leftdate">
-- |   01. 12. 2011
-- |   <br>
-- |   06. 01. 2012
-- | </div>
-- | <div>
-- |   <span>12:00</span>
-- |   <br>
-- |   <span>23:59</span>
-- | </div>
-- | <div>
-- |   <a href="online-campus/termine/...">Klausuran-/abmeldungen</a>
-- | </div>
-- |
-- | -> (([2011-12-01, 2012-01-06], [12:00, 23:59]), ("online-campus/termine/...", "Klausuran-/abmeldungen"))

getCalenderInfo :: IOSArrow XmlTree CalenderInfo
getCalenderInfo =
    getRelevantNodes
    >>>
    deep (isElem >>> hasName "div" >>> hasAttrValue "class" (== "tx-cal-controller "))
    >>>
    deep (isElem >>> hasName "dt")
    >>>
    (
      (
        (
          (deep (isElem >>> hasName "div" >>> hasAttrValue "class" (== "leftdate")) >>> extractText)
          >>^
          (dateRep2NormalizedDates . extractDateRep)
          >>^
          (\ dates -> if (not . null $ dates) then head dates else [])
        )
        &&&&
        (
          ((deep (isElem >>> hasName "div") >>> deep (isElem >>> hasName "span") >>> extractText) `withDefault` "")
        )
      )
      &&&
      (
        deep (isElem >>> hasName "div")
        >>>
        deep (isElem >>> hasName "a")
        >>>
        (
          (fromLA $ deep (getAttrValue "href"))
          &&&
          extractText
        )
      )
    )

-- ----------------------------------------------------------------------------
-- | Transform the CalenderInfo into a format that can be JSON-encoded and written into the
-- | page-info part of the index (see PageInfo.hs).
-- |
-- | Input:  CalenderInfo
-- | Output: One-element-list containing a list of hrefs, dates and teaserText found inside a calender-info
-- |
-- | Example:
-- | ((["2011-12-01-**-**", "2012-01-06-**-**"], ["12:00", "23:59"]), ("online-campus/termine/...", "Klausuran-/abmeldungen"))
-- | ->
-- | [
-- |  ["online-campus/termine/...", "01. Dezember 2011 12:00 - 06. Januar 2012 23:59" "Klausuran-/abmeldungen"],
-- |  ["online-campus/termine/...", "01. Dezember 2011 12:00 - 06. Januar 2012 23:59" "Klausuran-/abmeldungen"]
-- | ]
-- | 
-- | Although it seems strange that the same entry appears twice in the list, this will ensure that the entry is
-- | indexed twice: Once for the keyword "2011-12-01" and once for the keyword "2012-01-06".
-- | This is because calenderInfo2NormDates puts out a date for each entry of this list, when the keyword-part of the index is build
-- | (see IndexConfig.hs).
-- |
-- | Following combinations of dates and times with an associated link can appear:
-- | - one date, no time    : this is the start date of the event
-- | - one date, one time   : this is the start date with the start time of the event
-- | - one date, two times  : this is the start date with the start and stop time of the event
-- | - two dates, no times  : this is the start date and stop date of the event. This needs to be indexed twice (start adn stop date).
-- | - two dates, two times : this is the start date and time and stop date and time of the event. This needs to be indexed twice (start adn stop date).

calenderInfo2Context                                  :: CalenderInfo -> [[String]] -- [[href, date, teaserText]]
calenderInfo2Context ((dates, times), (href, teaser)) = mkCalenderEntry (length dates) (length times)
  where
    mkCalenderEntry 1 0 = [[href, (unNormalizeDate $ dates !! 0), teaser]]
    mkCalenderEntry 1 1 = [[href, (unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0), teaser]]
    mkCalenderEntry 1 2 = [[href, (unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0) ++ " - " ++
                                                                            (times !! 1), teaser]]
    mkCalenderEntry 2 0 = [[href, (unNormalizeDate $ dates !! 0) ++ " - " ++
                                  (unNormalizeDate $ dates !! 1), teaser],
                           [href, (unNormalizeDate $ dates !! 0) ++ " - " ++
                                  (unNormalizeDate $ dates !! 1), teaser]
                          ]
    mkCalenderEntry 2 2 = [[href, (unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0) ++ " - " ++
                                  (unNormalizeDate $ dates !! 1) ++ " " ++ (times !! 1), teaser],
                           [href, (unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0) ++ " - " ++
                                  (unNormalizeDate $ dates !! 1) ++ " " ++ (times !! 1), teaser]
                          ]
    mkCalenderEntry _ _ = [["", "", "unbekanntes Zeitformat im Kalender gefunden!"]]

-- ----------------------------------------------------------------------------
-- | Transform the CalenderInfo into a String containing all Dates found in the calender.
-- | This String will be assigned to the collectText of the calender-index (see IndexConfig.hs)
-- | The textToWords-function of the calender-index will build a list of normalized Dates, where each
-- | corresponds to an entry of the list returned by calenderInfo2Context.
-- |
-- | Input:  CalenderInfo
-- | Output: One-element-list containing a list of hrefs, dates and teaserText found inside a calender-info
-- |
-- | Example:
-- | ((["2011-12-01-**-**", "2012-01-06-**-**"], ["12:00", "23:59"]), ("online-campus/termine/...", "Klausuran-/abmeldungen"))
-- | ->
-- | "01. Dezember 2011 12:00 - 06. Januar 2012 23:59"

calenderInfo2NormDates         :: CalenderInfo -> String
calenderInfo2NormDates calInfo = concat . concat $ [map (head . tail) $ calenderInfo2Context calInfo]

-- ----------------------------------------------------------------------------
-- | Extract all text of an html or pdf document

getContentText :: IOSArrow XmlTree String
getContentText =  choiceA
                  [ isHtmlContents :-> getHtmlText
                  , isPdfContents  :-> (getAllText getChildren)
                  , this           :-> none
                  ]
       
-- ----------------------------------------------------------------------------
-- | Extract all text of an html document

getHtmlText :: IOSArrow XmlTree String
getHtmlText = getRelevantNodes >>> extractText

-- ----------------------------------------------------------------------------
-- | Find all text nodes, concatenate their contents, return a String

extractText :: IOSArrow XmlTree String
extractText = ( ( fromLA $ deep getText )
                >>^
                (" " ++)            -- text parts are separated by a space
              )
              >. (concat >>> normalizeWS)       -- normalize Space

-- ----------------------------------------------------------------------------
-- | Select all nodes whose contents are to be indexed.
-- | For some sites these are special nodes to ignore "useless" content (e.g. footers),
-- | for all other sites its simply the html-body node.
-- | This list could be further continued in future work.

getRelevantNodes                :: IOSArrow XmlTree XmlTree 
getRelevantNodes
    = selem "div"
      [ choiceA
        [ isFhwLayout     :-> ( traceMsg 1 "extract contents out of layout from FHW"
                                >>>
                                deep (hasDivWithId "col3_content")
                              )
        , isEgLayout      :-> ( traceMsg 1 "extract contents out of layout from Martin Egge"
                                >>>
                                ( deep (hasDivWithId "ContentHeaderDiv")
                                  <+> deep (hasDivWithId "ContentOutlineDiv")
                                  <+> deep (hasDivWithId "ContentBodyDiv")
                                -- <+> deep (hasDivWithId "SiteNavigationDiv")
                                )
                              )
        , isSiLayout      :-> ( traceMsg 1 "extract contents out of layout from Uwe"
                                >>>
                                deep (hasDivWithId "col2_content")
                              )
        , isSiLectureLayout
                          :-> ( traceMsg 1 "extract contents out of lecture layout from Uwe"
                                >>>
                                processTopDown
                                ( none
                                  `when`
                                  ( isElemWithAttr "div" "class" (== "navigate")
                                    <+>
                                    isElemWithAttr "table" "class" (== "fullwidth")
                                  )
                                )
                              )
        , isKiLayout      :-> ( traceMsg 1 "extract contents out of layout from Thorsten Kirch"
                                >>>
                                deep (hasDivWithId "main")
                              )
        , isPtlLayout     :-> ( traceMsg 1 "extract contents out of layout from PTL"
                                >>>
                                deep (hasDivWithId "col2_content")
                              )
        , this            :-> ( traceMsg 1 "extract contents out of layout from unknown layout"
                                >>>
                                getByPath ["html", "body"]
                              )
        ]
      ]

-- ------------------------------------------------------------
-- | Select the URI of the actual document

getURI                          :: ArrowXml a => a XmlTree String
getURI                          = fromLA $ getAttrValue transferURI

-- ------------------------------------------------------------
-- | select the modified date of a document

getModifiedAttr :: LA XmlTree String
getModifiedAttr
    = single
      ( getAttrValue0 "http-last-modified"          -- HTTP header
        <+>
        ( getMetaAttr "date" >>> isA (not . null) ) -- meta tag date (typo3)
        <+>
        getAttrValue0 "http-date"                   -- HTTP server time and date
        <+>
        constA ""
      )

-- ------------------------------------------------------------
-- 
-- predicate arrows
--  
-- ------------------------------------------------------------

-- ------------------------------------------------------------
-- | Filter sites of a certain layout.
-- | This list of functions could be further continued in future work.

isXxxLayout                     :: ArrowXml a => String -> a XmlTree XmlTree
isXxxLayout xxx                 = fromLA $
                                  getLink  -- hack
                                  >>>
                                  hasAttrValue "rel" (== "shortcut icon")
                                  >>>
                                  hasAttrValue "href" (== xxx)

isFhwLayout                     :: ArrowXml a => a XmlTree XmlTree
isFhwLayout                     = isXxxLayout "fileadmin/templates/fhw_images/favicon.ico"

isPtlLayout                     :: ArrowXml a => a XmlTree XmlTree
isPtlLayout                     = isXxxLayout "fileadmin/templates/ptl_images/favptlicon.ico"

isEgLayout                      :: ArrowXml a => a XmlTree XmlTree
isEgLayout                      = fromLA $
                                  ( getMetaAttr "author"
                                    >>>
                                    isA ("Martin Egge" `isPrefixOf`)
                                  )
                                  `guards` this

isSiLayout                      :: ArrowXml a => a XmlTree XmlTree
isSiLayout                      = fromLA $
                                  ( getMetaAttr "keywords"          -- hack: there should be a meta elem for author
                                    >>>
                                    isA ("Uwe Schmidt" `isPrefixOf`)
                                  )
                                  `guards` this

isSiLectureLayout               :: ArrowXml a => a XmlTree XmlTree
isSiLectureLayout               = fromLA $
                                  ( getMetaAttr "author"
                                    >>>
                                    isA (== "Uwe Schmidt")
                                  ) `guards`
                                  ( ( getByPath ["html", "body"]
                                      >>>
                                      hasAttrValue "id" (== "lecture")
                                    ) 
                                    `guards` this
                                  )

isKiLayout                      :: ArrowXml a => a XmlTree XmlTree
isKiLayout                      = fromLA $
                                  ( getMetaAttr "author"
                                    >>>
                                    isA ("Thorsten Kirch" `isPrefixOf`)
                                  )
                                  `guards` this

-- ------------------------------------------------------------
--
-- predicate filter with document age

hasDocumentAgeWith:: (String -> String -> Bool) -> Int -> IOSArrow XmlTree XmlTree
hasDocumentAgeWith cmp ageInDays
    = ( fromLA getModifiedAttr
        >>>
        ( arrIO $ cmpDate cmp (toInteger ageInDays) )
        >>>
        isA id
      )
      `guards` this

isNewerThan :: Int -> IOSArrow XmlTree XmlTree
isNewerThan ageInDays
    = hasDocumentAgeWith (>=) ageInDays
      >>>
      traceMsg 0 ("document as new classified (modified within the last " ++ show ageInDays ++ " days)")

isOldStuff :: Int -> IOSArrow XmlTree XmlTree
isOldStuff ageInDays
    = ( hasDocumentAgeWith (<) ageInDays
        `orElse`
        isArchiveDoc
      )
      >>>
      traceMsg 0 ("document as old stuff classified (older than " ++ show ageInDays ++ " days) or in archive")

isArchiveDoc :: IOSArrow XmlTree XmlTree
isArchiveDoc
    = ( getURI >>> isA (match ".*/(veranstaltungs)?[Aa]rchiv(e)?/.*") )
      `guards`
      ( this
        >>>
        traceMsg 0 "archive document found"
      )

isWolterJunk :: IOSArrow XmlTree XmlTree
isWolterJunk
    = ( getURI >>> isA (match ".*/wol/.*/(BESCHAFFUNG|PLANKARTE).*\\.pdf") )
      `guards`
      ( this
        >>>
        traceMsg 0 ("Birger Wolters Abas Junk with lots of useless dates found")
      )

-- ------------------------------------------------------------
--
-- mothers little helpers
--
-- ------------------------------------------------------------

hasNameWithId                   :: ArrowXml a => String -> String -> a XmlTree XmlTree
hasNameWithId ename eid         = isElem
                                  >>>
                                  hasName ename
                                  >>>
                                  hasAttrValue "id" (== eid)

hasDivWithId                    :: ArrowXml a => String -> a XmlTree XmlTree
hasDivWithId                    = hasNameWithId "div"

getMeta                         :: ArrowXml a => a XmlTree XmlTree
getMeta                         = getByPath ["html", "head", "meta"]

getLink                         :: ArrowXml a => a XmlTree XmlTree
getLink                         = getByPath ["html", "head", "link"]

getMetaAttr                     :: ArrowXml a => String -> a XmlTree String
getMetaAttr key                 = getMeta
                                  >>>
                                  hasAttrValue "name" ((== key) . map toLower)
                                  >>>
                                  getAttrValue "content"


-- all strings with length < 2 are boring
-- and all strings not starting with a letter
boringWord                      :: String -> Bool
boringWord w                    = null w
                                  ||
                                  (null . tail $ w)
                                  ||
                                  not (any isXmlLetter w)

boringURIpart                   :: String -> Bool
boringURIpart                   = ( `elem`
                                    [ ""
                                    , "http", "www", "wwwab", "fh-wedel", "ptl", "de"
                                    , "html", "pdf"
                                    , "mitarbeiter", "fileadmin"
                                    ]
                                  )

-- ------------------------------------------------------------
--
-- text preprocessing
--
-- ------------------------------------------------------------

deleteNotAllowedChars           :: String -> String
deleteNotAllowedChars           = map notAllowedToSpace
    where
    notAllowedToSpace c
        | isAllowedWordChar c   = c
        | otherwise             = ' '

isAllowedWordChar   :: Char -> Bool
isAllowedWordChar c = isXmlLetter c
                      ||
                      isXmlDigit c
                      ||
                      c `elem` "_-"

-- ------------------------------------------------------------
-- | tokenize a uri string

uri2Words :: String -> [String]
uri2Words s
    = tildeMitarbeiter s
      ++
      tokenize "[^:/#?=.]+" s

tildeMitarbeiter :: String -> [String]
tildeMitarbeiter
    = map ("~" ++) .
      drop 1 .
      tokenize "[a-z]+" .
      concat .
      tokenize "/mitarbeiter/[a-z]{2,3}/"

-- ------------------------------------------------------------

