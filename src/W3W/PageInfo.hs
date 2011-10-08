{-# OPTIONS #-}

module W3W.PageInfo
    ( PageInfo(..)
    , mkPageInfo
    , emptyPageInfo
    , w3wGetTitle
    , w3wGetPageInfo
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
                                ( fromLA (getModified `withDefault` "kein teaser für modified gefunden!")
                                  &&&
                                  fromLA (getAuthor `withDefault` "kein teaser für author gefunden!")
                                  &&&
                                  (getTeaserTextPageCont `withDefault` "kein teaser für content gefunden!")
                                  &&&
                                  ((getTeaserTextDates dateExtractor dateProcessor) `withDefault` "kein teaser für dates gefunden!")
                                  &&&
                                  ((getTeaserTextCalender dateExtractor dateProcessor) `withDefault` "kein teaser für kalender gefunden!")
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

getTeaserTextCalender :: D.DateExtractorFunc -> D.DateProcessorFunc -> IOSArrow XmlTree String
getTeaserTextCalender dateExtractor dateProcessor =
                        (
                          getRelevantNodes
                          >>>
                          deep (isElem >>> hasName "table" >>> hasAttrValue "class" (== "month-large"))
                          >>>
                          deep (isElem >>> hasName "a")
                          >>>
                          (
                            (fromLA $ deep (getAttrValue "href"))
                            &&&
                            extractText
                          )
                          >>^
                          hrefAndText2CalenderContext
                        )
                        >.
                        concat
                        >>^
                        toJSONArray
                        where
                          hrefAndText2CalenderContext (href, text) =
                            let res = (getNormFunc D.dateRep2NormalizedDates . dateExtractor $ href) in
                              [[ href
                              , if (not . null $ res) then D.unNormalizeDate . head $ res else "-"
                              , text
                              ]]


toJSONArray :: [[String]] -> String
toJSONArray = encodeStrict . showJSONs

-- ------------------------------------------------------------

