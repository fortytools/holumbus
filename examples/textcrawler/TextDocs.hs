{-# OPTIONS #-}

-- ------------------------------------------------------------

module TextDocs
where

import qualified Data.Map as M

import           Holumbus.Crawler.HtmlText

import		 Text.XML.HXT.Arrow		hiding ( when
						       , getState
						       )

-- ------------------------------------------------------------

xpTextDocs		:: String -> PU TextDocs
xpTextDocs ref		= xpElem "html" $
			  xpWrap ( snd
				 , \ x -> ("Content of HTML Pages of " ++ show ref, x)
				 ) $
			  xpPair ( xpElem "head" $
				   xpElem "title" $
				   xpText
				 )
			         ( xpElem "body" $
				   xpWrap ( snd
					  , \ x -> ("Content of HTML Pages of " ++ show ref, x)
					  ) $
				   xpPair ( xpElem "h1" $
					    xpText
					  )
				          ( xpWrap ( M.fromList
						   , M.toList
						   ) $
					    xpList $
					    xpPair ( xpElem "dt" $
						     xpElem "a" $
						     xpWrap ( snd
							    , \ x -> (x, x)
							    ) $
						     xpPair (xpAttr "href" $ xpText) xpText
						   )
                                            ( xpElem "dd" $ xpText )
					  )
				 )

-- ------------------------------------------------------------
