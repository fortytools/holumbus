{-# OPTIONS #-}

module W3W.PageInfo
    ( PageInfo(..)
    , mkPageInfo
    , emptyPageInfo
    , w3wGetTitle
    , w3wGetPageInfo
    , (&&&&)
    )

where
import           	Control.DeepSeq
import 		 		    Control.Monad			( liftM4, liftM5 )
import           	Data.Binary                    ( Binary(..) )
import qualified 	Data.Binary                    as B
import           	Data.List                      ( isPrefixOf )
import		 		    Holumbus.Crawler
import		 		    Text.XML.HXT.Core
import           	W3W.Extract
import	          W3W.Date as D
import        		Text.Regex.XMLSchema.String (tokenizeExt)
import        		Text.JSON


-- ------------------------------------------------------------

-- | Additional information about a function.

data PageInfo = PageInfo
    { modified 	      :: String      -- ^ The last modified timestamp
    , author  	      :: String      -- ^ The author
    , contentContext  :: String      -- ^ The first few lines of the page contents
    , datesContext    :: String      -- ^ The dates
    , calenderContext :: String      -- ^ The dates
    }
    deriving (Show, Eq)

mkPageInfo 			:: String -> String -> String -> String -> String -> PageInfo
mkPageInfo			= PageInfo

emptyPageInfo   :: PageInfo
emptyPageInfo   = mkPageInfo "" "" "" "" ""


instance XmlPickler PageInfo where
  xpickle = xpWrap (fromTuple, toTuple) xpFunction
    where
      fromTuple (m, a, c, d, x) = PageInfo m a c d x
      toTuple (PageInfo m a c d x) = (m, a, c, d, x)
      xpFunction = xp5Tuple xpModified xpAuthor xpContent xpDates xpCalender
        where -- We are inside a doc-element, and everything is stored as attribute.
          xpModified    = xpAttr "modified" xpText0
          xpAuthor      = xpAttr "author"   xpText0
          xpContent     = xpAttr "contentContext"  xpText0
          xpDates       = xpAttr "datesContext"    xpText0
          xpCalender    = xpAttr "calenderContext" xpText0

instance NFData PageInfo where
    rnf (PageInfo m a c d x) = rnf m `seq` rnf a `seq` rnf c `seq` rnf d `seq` rnf x `seq` ()

instance B.Binary PageInfo where
    put (PageInfo m a c d x) = put m >> put a >> put c >> put d >> put x
    get = do
            r <- liftM5 PageInfo get get get get get
            rnf r `seq` return r

-- ------------------------------------------------------------
--
-- the arrows for building the document descr
--
-- ------------------------------------------------------------

w3wGetTitle                     :: IOSArrow XmlTree String
w3wGetTitle                     = fromLA $
                                  ( getHtmlTitle		-- title contents
                                    >>>
                                    isA (not . null)    -- if empty, take the URI as title
                                  )
                                  `orElse`
                                  getURI


w3wGetPageInfo                  :: D.DateExtractorFunc -> D.DateProcessorFunc -> IOSArrow XmlTree PageInfo
w3wGetPageInfo dateExtractor dateProcessor =
                                ( fromLA (getModified `withDefault` "")
                                  &&&
                                  fromLA (getAuthor `withDefault` "")
                                  &&&
                                  (getTeaserTextPageCont `withDefault` "")
                                  &&&
                                  ((getTeaserTextDates dateExtractor dateProcessor) `withDefault` "")
                                  &&&
                                  ((getTeaserTextCalender dateExtractor dateProcessor) `withDefault` "")
                                )
                                >>^
                                (\ (m, (a, (c, (d, x)))) -> mkPageInfo m a c d x)

getModified                     :: LA XmlTree String
getModified                     = ( getAttrValue0 "http-last-modified"		-- HTTP header
                                    `orElse`
                                    ( getMetaAttr "date" >>> isA (not . null) ) -- meta tag date (typo3)
                                    `orElse`
                                    getAttrValue0 "http-date"                    -- HTTP server time and date
                                  )
                                  >>^
                                  normalizeDateModified

getAuthor                       :: LA XmlTree String
getAuthor                       = getAuthorFromURI
                                  `orElse`
                                  getAuthorFromContent
    where
    getAuthorFromURI		= single
                                  ( getURI
                                    >>>
                                    arrL uri2Words
                                    >>>
                                    isA ("~" `isPrefixOf`)
                                    >>>
                                    arr short2Name
                                    >>>
                                    isA (not . null)
                                  )
    getAuthorFromContent        = none		-- not yet implemented


getTeaserTextPageCont :: IOSArrow XmlTree String
getTeaserTextPageCont =
{-
            (
              trace 1 $
              getHtmlText
            )
            >>>
-}
            getContentText
              >>^
              (words >>> unwordsCont 50)


            -- take the first 50 words from content

getTeaserTextDates :: D.DateExtractorFunc -> D.DateProcessorFunc -> IOSArrow XmlTree String
getTeaserTextDates dateExtractor dateProcessor =
            getContentText
            >>^
            (
              (getContextFunc dateProcessor . dateExtractor)
              >>>
              toJSONArray
            )

getFirstIfDef [] = []
getFirstIfDef (x:xs) = [x]

(&&&&) :: IOSLA t t1 t2 -> IOSLA t t1 t3 -> IOSLA t t1 ([t2], [t3])
IOSLA f &&&& IOSLA g = IOSLA $ \ s x -> do
                                        (s1, ys1) <- f s  x
                                        (s2, ys2) <- g s1 x
                                        return ( s2, [ ( [y1 | y1 <- ys1], [y2 | y2 <- ys2] ) ] )

getTeaserTextCalender :: D.DateExtractorFunc -> D.DateProcessorFunc -> IOSArrow XmlTree String
getTeaserTextCalender dateExtractor dateProcessor =
{-  
  (
    getRelevantNodes
    >>>
    deep (isElem >>> hasName "div" >>> hasAttrValue "class" (== "tx-cal-controller "))
    >>>
    deep (isElem >>> hasName "dt")
    >>>
    (
      (deep (isElem >>> hasName "div" >>> hasAttrValue "class" (== "leftdate")) >>> extractText)
      &&&
      ((deep (isElem >>> hasName "div") >>> deep (isElem >>> hasName "span") >>> extractText) `withDefault` "")
    )
    >>^ ( \ (a,b) -> a ++ " " ++ b )
  )
  >.
  concat
  >>^
  (
    (getContextFunc dateProcessor . dateExtractor)
    >>>
    toJSONArray
  )
  -}
  -- ! Datum trennen und mit zeit kombinieren !
  
  (
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
          (getNormFunc D.dateRep2NormalizedDates . dateExtractor)
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
    >>^ ( \ ((dates', times),(href, teaser)) -> 
          let
            dates = if (not . null $ dates') then head dates' else ["no date"]
            mkCalenderEntry 1 0 = [[href, (D.unNormalizeDate $ dates !! 0), teaser]]
            mkCalenderEntry 1 1 = [[href, (D.unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0), teaser]]
            mkCalenderEntry 1 2 = [[href, (D.unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0) ++ " - " ++
                                                                                    (times !! 1), teaser]]
            mkCalenderEntry 2 0 = [[href, (D.unNormalizeDate $ dates !! 0) ++ " - " ++
                                          (D.unNormalizeDate $ dates !! 1), teaser],
                                   [href, (D.unNormalizeDate $ dates !! 0) ++ " - " ++
                                          (D.unNormalizeDate $ dates !! 1), teaser]
                                  ]
            mkCalenderEntry 2 2 = [[href, (D.unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0) ++ " - " ++
                                          (D.unNormalizeDate $ dates !! 1) ++ " " ++ (times !! 1), teaser],
                                   [href, (D.unNormalizeDate $ dates !! 0) ++ " " ++ (times !! 0) ++ " - " ++
                                          (D.unNormalizeDate $ dates !! 1) ++ " " ++ (times !! 1), teaser]
                                  ]
            mkCalenderEntry _ _ = [["", "", "unbekanntes Zeitformat im Kalender gefunden!"]]
          in
            mkCalenderEntry (length dates) (length times)
        )
  )
  >.
  concat
  >>^
  toJSONArray

 
toJSONArray :: [[String]] -> String
toJSONArray = encodeStrict . showJSONs

-- ------------------------------------------------------------

