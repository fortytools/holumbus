{-# OPTIONS #-}

-- ------------------------------------------------------------

module W3W.PageInfo
    ( PageInfo(..)
    , mkPageInfo
    , w3wGetTitle
    , w3wGetPageInfo
    )
where
import           Control.DeepSeq
import 		 Control.Monad			( liftM3 )

import           Data.Binary                    ( Binary(..) )
import qualified Data.Binary                    as B
import           Data.List                      ( isPrefixOf )

import		 Holumbus.Crawler

import		 Text.XML.HXT.Core

import           W3W.Extract

-- ------------------------------------------------------------

-- | Additional information about a function.

data PageInfo 		        = PageInfo 
    				  { modified 	:: String      		-- ^ The last modified timestamp
				  , author 	:: String       	-- ^ The author
                                  , content     :: String               -- ^ The first few lines of the page contents
				  } 
				  deriving (Show, Eq)

mkPageInfo 			:: String -> String -> String -> PageInfo
mkPageInfo			= PageInfo

instance XmlPickler PageInfo where
    xpickle 			= xpWrap (fromTuple, toTuple) xpFunction
	where
	fromTuple (m, a, c)
			 	= PageInfo m a c
	toTuple (PageInfo m a c)
				= (m, a, c)

	xpFunction		= xpTriple xpModified xpAuthor xpContent
	    where 							-- We are inside a doc-element, and everything is stored as attribute.
	    xpModified 		= xpAttr "modified" xpText0
	    xpAuthor     	= xpAttr "author"   xpText0
	    xpContent 		= xpAttr "content"  xpText0

instance NFData PageInfo where
    rnf (PageInfo m a c)	= rnf m `seq` rnf a `seq` rnf c `seq` ()

instance B.Binary PageInfo where
    put (PageInfo m a c)
			 	= put m >> put a >> put c
    get 			= do
				  r <- liftM3 PageInfo get get get
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

w3wGetPageInfo                  :: IOSArrow XmlTree PageInfo
w3wGetPageInfo                  = ( fromLA (getModified `withDefault` "")
                                    &&&
                                    fromLA (getAuthor `withDefault` "")
                                    &&&
                                    getPageCont
                                  )
                                  >>^
                                  (\ (m, (a, c)) -> mkPageInfo m a c)

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

getPageCont                     :: IOSArrow XmlTree String
getPageCont                     = getHtmlText
                                  >>^
                                  (words >>> unwordsCont 100)	-- take the first 100 words from content

-- ------------------------------------------------------------