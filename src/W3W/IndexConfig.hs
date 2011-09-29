{-# OPTIONS #-}

-- ------------------------------------------------------------

module W3W.IndexConfig
where

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
import           Text.XML.HXT.Core
import           W3W.Extract
import        	 W3W.Date as D
-- ------------------------------------------------------------

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
                                  , ixc_collectText     = getRelevantNodes
                                                          >>>
                                                          deep (isElem >>> hasName "table" >>> hasAttrValue "class" (== "month-large"))
                                                          >>>
                                                          deep (isElem >>> hasName "a")
                                                          >>>
                                                          (fromLA $ deep (getAttrValue "href"))

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
                                  , ixc_collectText     = getHtmlText
                                  , ixc_textToWords     = getNormFunc dateProcessor . dateExtractor
                                  , ixc_boringWord      = null
                                  }

    ixContent                   = ixDefault
                                  { ixc_name            = "content"
                                  , ixc_collectText     = getContentText
                                  }
-- -----------------------------------------------------------------------------
