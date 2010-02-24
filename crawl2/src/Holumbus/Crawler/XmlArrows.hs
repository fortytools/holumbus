{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.XmlArrows
where

import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

-- | Remove contents, when document status isn't ok, but remain meta info

checkDocumentStatus		:: IOSArrow XmlTree XmlTree
checkDocumentStatus		=  replaceChildren none
				   `whenNot`
				   documentStatusOk

-- ------------------------------------------------------------

