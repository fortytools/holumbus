{-# OPTIONS #-}

-- ------------------------------------------------------------

module IndexConfig
where

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
import           Text.XML.HXT.Core
import           Extract
import           Date as D
-- ------------------------------------------------------------

w3wIndexConfig           :: [IndexContextConfig]
w3wIndexConfig
                                = [
                                    ixHeadlines
                                  , ixURI
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
                                  , ixc_collectText     = getCalenderInfo >>^ calenderInfo2NormDates
                                  , ixc_textToWords     = D.dateRep2NormalizedDates . D.extractDateRep
                                  , ixc_boringWord      = null
                                  }

    ixDates                     = ixDefault
                                  { ixc_name            = "dates"
                                  , ixc_collectText     = getContentText
                                  , ixc_textToWords     = D.dateRep2NormalizedDates . D.extractDateRep
                                  , ixc_boringWord      = null
                                  }

    ixContent                   = ixDefault
                                  { ixc_name            = "content"
                                  , ixc_collectText     = getContentText
                                  }
-- -----------------------------------------------------------------------------
