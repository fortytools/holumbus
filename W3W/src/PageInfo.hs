-- ----------------------------------------------------------------------------

{- |
  Module     : PageInfo

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Definition of all information that is collected per indexed site
  and stored in the index file (ix.bin/ix.bin.xml).
  This includes information about the site author,
  the modified-date of a site and the context-info.
  The context-info for an indexed word
  consists of the first 50 words of the site.
  For dates, the context-info conists of the 5 previous words
  and the 5 next words surrouning the date.
  For dates found in the FH Wedel-calender,
  the context-info consists of the href-information
  (the link and the text node of an anchor tag)
  associated with that date.
-}

-- ----------------------------------------------------------------------------
{-# OPTIONS #-}

module PageInfo
    ( PageInfo(..)
    , mkPageInfo
    , emptyPageInfo
    , w3wGetTitle
    , w3wGetPageInfo
    )

where
import            Control.DeepSeq
import            Control.Monad                  ( liftM5 )

import            Data.Binary                    ( Binary(..) )
import qualified  Data.Binary                    as B
import            Data.List                      ( isPrefixOf )
import            Data.Maybe

import            Holumbus.Crawler

import            Text.JSON
import            Text.XML.HXT.Core

import            Extract
import            Date                          as D


-- ------------------------------------------------------------
-- | All parts of the page-info associated to an indexed website

data PageInfo = PageInfo
    { modified        :: String      -- ^ The last modified timestamp
    , author          :: String      -- ^ The author
    , contentContext  :: String      -- ^ The first few lines of the page contents
    , datesContext    :: String      -- ^ The dates
    , calenderContext :: String      -- ^ The dates
    }
    deriving (Show, Eq)

emptyPageInfo   :: PageInfo
emptyPageInfo   = mkPageInfo "" "" "" "" ""

mkPageInfo  :: String -> String -> String -> String -> String -> PageInfo
mkPageInfo  = PageInfo

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
    rnf (PageInfo m a c d x) = rnf m `seq` rnf a `seq` rnf c `seq` rnf d `seq` rnf x

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

-- ------------------------------------------------------------
-- | the title of a document.
-- | returns the URI if the title is not defined

w3wGetTitle :: IOSArrow XmlTree String
w3wGetTitle = fromLA $
              ( getHtmlTitle        -- title contents
                >>>
                isA (not . null)    -- if empty, take the URI as title
              )
              `orElse`
              getURI

-- ------------------------------------------------------------
-- | the PageInfo of a document.
-- | This includes information about the site author, the modified-date of a site and the context-info.

w3wGetPageInfo :: IOSArrow XmlTree PageInfo
w3wGetPageInfo =
                ( fromLA (getModified `withDefault` "")
                  &&&
                  (getAuthor `withDefault` "")
                  &&&
                  (getContextInfoPageCont `withDefault` "") -- The context-info for an indexed word consists of the first 50 words of the documents content.
                  &&&
                  (getContextInfoDates `withDefault` "") -- For dates, the context-info conists of the 5 previous words and the 5 next words surrouning the date.
                  &&&
                  (getContextInfoCalender `withDefault` "") -- For dates found in the FH Wedel-calender, the context-info consists of the href-information (the link and the text node of an anchor tag) associated with that date.
                )
                >>^
                (\ (m, (a, (c, (d, x)))) -> mkPageInfo m a c d x)

-- ------------------------------------------------------------
-- | These abbreviations are used to determine the author of a site by its
-- | URI. Used in PageInfo.hs/getAuthor.
-- | This list should be kept up to date in future work.
-- | Other authors are found by seeking for a certain tag containing their name (see PageInfo.hs/getAuthor).

short2Name      :: String -> String
short2Name sn                   = fromMaybe "" . lookup (drop 1 sn) $
                                  [ ("rb", "Ulrich Raubach")
                                  , ("ur", "Sven Urbanski")
                                  , ("eg", "Martin Egge")
                                  , ("si", "Uwe Schmidt")
                                  , ("ahr", "Dirk Ahrens")
                                  , ("an", "Michael Anders")
                                  , ("bd", "Rene Bodaine")
                                  , ("ce", "Michael Ceyp")
                                  , ("ge", "Hans-Detlef Gerhardt")
                                  , ("hs", "Andreas Häuslein")
                                  , ("uh", "Ulrich Hoffmann")
                                  , ("iw", "Sebastian Iwanowski")
                                  , ("kal", "Ilja Kaleck")
                                  , ("rb", "Ulrich Raubach")
                                  , ("uw", "Wolfgang Uelzmann")
                                  , ("bos", "Timm Borstelmann")
                                  , ("eg", "Martin Egge")
                                  , ("klk", "Gerit Kaleck")
                                  , ("kar", "Helga Karafiat")
                                  , ("ki", "Thorsten Kirch")
                                  , ("ne", "Lars Neumann")
                                  , ("op", "Dieter Opitz")
                                  , ("uhl", "Christian Uhlig")
                                  ]


-- ------------------------------------------------------------
-- | Normalize the modified-dates to human readable representations.
-- | Used in PageInfo.hs/getModified

normalizeDateModified         :: String -> String
normalizeDateModified rawDate
    = if (not . null $ normalizedDates)
      then head normalizedDates
      else rawDate
    where
      normalizedDates = (map unNormalizeDate) . dateRep2NormalizedDates . extractDateRep $ rawDate

-- ------------------------------------------------------------
-- | like unwords but with a limited number of words (appends "...")

unwordsCont                     :: Int -> [String] -> String
unwordsCont mx ws
    | length ws' > mx           = unwords . (++ ["..."]) . init $ ws'
    | otherwise                 = unwords                       $ ws'
    where
    ws'                         = take (mx + 1) ws

-- ------------------------------------------------------------
-- | select the modified date of a document and put it in a
-- | normalized form (if recognized by date parser, see Date.hs)

getModified :: LA XmlTree String
getModified =
  ( getAttrValue0 "http-last-modified"          -- HTTP header
    `orElse`
    ( getMetaAttr "date" >>> isA (not . null) ) -- meta tag date (typo3)
    `orElse`
    getAttrValue0 "http-date"                   -- HTTP server time and date
  )
  >>^
  normalizeDateModified

-- ------------------------------------------------------------
-- | select the author of a document.

getAuthor :: IOSArrow XmlTree String
getAuthor
    = fromLA (getAuthorFromURI)
      `orElse`
      getAuthorFromContent
    where
      -- authors are identified by URIs, if they are formed like ".../~xy/..."
      getAuthorFromURI
          = single
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
      -- authors are identified by a certain html-node containing their name, if they used a common typo3-template
      getAuthorFromContent
          = getRelevantNodes
            >>>
            deep (isElem >>> hasName "div" >>> hasAttrValue "class" (== "fhwmitarbeiterinfos_contentleft"))
            >>>
            deep (isElem >>> hasName "h1" >>> hasAttrValue "class" (== "csc-firstHeader"))
            >>>
            extractText

getContextInfoPageCont :: IOSArrow XmlTree String
getContextInfoPageCont =
  getContentText
  >>^
  (words >>> unwordsCont 50)

getContextInfoDates :: IOSArrow XmlTree String
getContextInfoDates =
  getContentText
  >>^
  (
    (D.dateRep2DatesContext . D.extractDateRep)
    >>>
    toJSONArray
  )

getContextInfoCalender :: IOSArrow XmlTree String
getContextInfoCalender =
  (
    getCalenderInfo
    >>^
    calenderInfo2Context
  )
  >.
  concat
  >>^
  toJSONArray

 
toJSONArray :: [[String]] -> String
toJSONArray = encodeStrict . showJSONs

-- ------------------------------------------------------------

