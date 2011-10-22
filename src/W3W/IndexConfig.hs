{-# OPTIONS #-}

-- ------------------------------------------------------------

module W3W.IndexConfig
where

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
import           Text.XML.HXT.Core
import           W3W.Extract
import           W3W.Date as D
-- ------------------------------------------------------------

getFirstIfDef [] = []
getFirstIfDef (x:xs) = [x]

(&&&&) :: IOSLA t t1 t2 -> IOSLA t t1 t3 -> IOSLA t t1 ([t2], [t3])
IOSLA f &&&& IOSLA g = IOSLA $ \ s x -> do
                                        (s1, ys1) <- f s  x
                                        (s2, ys2) <- g s1 x
                                        return ( s2, [ ([y1 | y1 <- ys1], [y2 | y2 <- ys2]) ] )

w3wIndexConfig           :: D.DateExtractorFunc -> D.DateProcessorFunc -> [IndexContextConfig]
w3wIndexConfig dateExtractor dateProcessor
                                = [
                                    ixHeadlines
                                  , ixURI
                                  , ixURIClass
                                  , ixDates
                                  , ixContent
                                  , ixCalender
                                  ]
    where
    ixDefault                   = IndexContextConfig
                                  { ixc_name            = "default"
                                  , ixc_collectText     = getHtmlPlainText
                                  , ixc_textToWords     = deleteNotAllowedChars >>> words
                                  , ixc_boringWord      = boringWord
                                  }

    ixHeadlines                 = ixDefault
                                  { ixc_name            = "headline"
                                  , ixc_collectText     = getHeadlines
                                  }

    ixURI                       = ixDefault
                                  { ixc_name            = "uri"
                                  , ixc_collectText     = getURI
                                  , ixc_textToWords     = uri2Words     -- split uri path at /
                                  , ixc_boringWord      = boringURIpart
                                  }

    ixCalender                  = ixDefault
                                  { ixc_name            = "calender"
                                  , ixc_collectText     =
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
            mkCalenderEntry 1 0 = (D.unNormalizeDate $ dates !! 0)
            mkCalenderEntry 1 1 = (D.unNormalizeDate $ dates !! 0)
            mkCalenderEntry 1 2 = (D.unNormalizeDate $ dates !! 0)
            mkCalenderEntry 2 0 = (D.unNormalizeDate $ dates !! 0) ++ " " ++ (D.unNormalizeDate $ dates !! 1)
            mkCalenderEntry 2 2 = (D.unNormalizeDate $ dates !! 0) ++ " " ++ (D.unNormalizeDate $ dates !! 1)
            mkCalenderEntry _ _ = "unbekanntes Zeitformat im Kalender gefunden!"
          in
            mkCalenderEntry (length dates) (length times)
        )
  )

                                   


                                  , ixc_textToWords     = getNormFunc dateProcessor . dateExtractor
                                  , ixc_boringWord      = boringURIpart
                                  }

    ixURIClass                  = ixDefault
                                  { ixc_name            = "uriclass"
                                  , ixc_collectText     = getURI
                                  , ixc_textToWords     = uri2Words >>> classifyURIword
                                  , ixc_boringWord      = null
                                  }

    ixDates                     = ixDefault
                                  { ixc_name            = "dates"
                                  , ixc_collectText     = getContentText
                                  , ixc_textToWords     = getNormFunc dateProcessor . dateExtractor
                                  , ixc_boringWord      = null
                                  }

    ixContent                   = ixDefault
                                  { ixc_name            = "content"
                                  , ixc_collectText     = getContentText
                                  }
-- -----------------------------------------------------------------------------
