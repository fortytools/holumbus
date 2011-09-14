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

w3wIndexContextConfig           :: D.DateExtractorFunc -> D.DateProcessorFunc -> [IndexContextConfig]
w3wIndexContextConfig dateExtractor dateProcessor
                                = [
									                  ixHeadlines
                                  , ixURI
                                  , ixURIClass
                                  , ixDates
                                  , ixHrefDates
                								  , ixContent
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

    ixHrefDates                  = ixDefault
                                  { ixc_name            = "hrefdates"
                                  , ixc_collectText     =  getByPath ["html", "body"] -- weiter eingrenzen
                                                           >>>
                                                           deep (isElem >>> hasName "table" >>> hasAttrValue "class" (== "month-large"))
                                                           >>>
                                                           deep (isElem >>> hasName "td")
                                                           >>>
                                                           deep (isElem >>> hasName "div")
                                                           >>>
                                                           deep (isElem >>> hasName "a")
                                                           >>>
                                                           getAttrValue "href"
                                                           {-
                                                           (getAttrValue "href"
                                                             &&&
                                                            getHtmlPlainText)
                                                            >>> arr2 (\ a b -> a ++ " <<< " ++ b ++ " >>> ")
                                                            -}
                                  , ixc_textToWords     = dateProcessor . dateExtractor
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
                                  , ixc_collectText     = getHtmlPlainText
                                                          >>^
                                                          (words >>> unwords)
                                  , ixc_textToWords     = dateProcessor . dateExtractor
                                  , ixc_boringWord      = null
                                  }

    ixContent                   = ixDefault
                                  { ixc_name            = "content"
                                  , ixc_collectText     = getContentText
                                  }
-- -----------------------------------------------------------------------------
