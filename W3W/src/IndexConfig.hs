-- ----------------------------------------------------------------------------

{- |
  Module     : IndexConfig

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The List of sub indexes created in the index file
-}

-- ----------------------------------------------------------------------------

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
-- | The List of sub indexes created in the index file (ix.bin / ix.bin.xml)

w3wIndexConfig :: [IndexContextConfig]
w3wIndexConfig
    = [ ixHeadlines
      , ixURI
      , ixDates
      , ixContent
      , ixCalender
      ]
    where
    ixDefault
        = IndexContextConfig
          { ixc_name            = "default"
          , ixc_collectText     = getHtmlPlainText
          , ixc_textToWords     = deleteNotAllowedChars >>> words
          , ixc_boringWord      = boringWord
          }

    -- index for all words found in headlines (h1-h6 tags)
    ixHeadlines
        = ixDefault
          { ixc_name            = "headline"
          , ixc_collectText     = getHeadlines
          }

    -- index for all words found in URIs ignoring boring parts like "http", "www", etc.
    ixURI
        = ixDefault
          { ixc_name            = "uri"
          , ixc_collectText     = getURI
          , ixc_textToWords     = uri2Words     -- split uri path at /
          , ixc_boringWord      = boringURIpart
          }

    -- index for all dates found in the calender site (http://www.fh-wedel.de/online-campus/termine/)
    ixCalender
        = ixDefault
          { ixc_name            = "calender"
          , ixc_collectText     = getCalenderInfo >>^ calenderInfo2NormDates
          , ixc_textToWords     = D.dateRep2NormalizedDates . D.extractDateRep
          , ixc_boringWord      = null
          }

    -- index for all dates found in any site
    ixDates
        = ixDefault
          { ixc_name            = "dates"
          , ixc_collectText     = getContentText
          , ixc_textToWords     = D.dateRep2NormalizedDates . D.extractDateRep
          , ixc_boringWord      = null
          }

    -- index for all words found in any site
    ixContent
        = ixDefault
          { ixc_name            = "content"
          , ixc_collectText     = getContentText
          }

-- -----------------------------------------------------------------------------
