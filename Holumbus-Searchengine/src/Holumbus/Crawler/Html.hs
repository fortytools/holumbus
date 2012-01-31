{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Html
where

import           Data.Function.Selector

import           Data.List
import           Data.Maybe

import           Holumbus.Crawler.Types
import           Holumbus.Crawler.URIs

import           System.FilePath

import           Text.XML.HXT.Core              hiding ( when
                                                       , getState
                                                       )
{- just for debugging
import qualified Debug.Trace                    as D
-- -}
-- ------------------------------------------------------------

defaultHtmlCrawlerConfig        :: AccumulateDocResult a r ->
                                   MergeDocResults r -> CrawlerConfig a r
defaultHtmlCrawlerConfig op op2 = ( setS theSysConfig
                                             ( withValidate no
                                               >>>
                                               withParseHTML yes
                                               >>>
                                               withInputEncoding isoLatin1
                                               >>>
                                               withWarnings no
                                               >>>
                                               withIgnoreNoneXmlContents yes
                                             )
                                    >>>
                                    setS thePreRefsFilter this
                                    >>>
                                    setS theProcessRefs getHtmlReferences
                                    $ 
                                    defaultCrawlerConfig op op2
                                  )

-- ------------------------------------------------------------

-- | Collect all HTML references to other documents within a, frame and iframe elements

getHtmlReferences               :: ArrowXml a => a XmlTree URI
getHtmlReferences               = fromLA (getRefs $< computeDocBase)
    where
    getRefs base                = deep (hasNameWith
                                        ( (`elem` ["a","frame","iframe"]) . localPart )
                                       )
                                  >>>
                                  ( getAttrValue0 "href"
                                    <+>
                                    getAttrValue0 "src"
                                  )
                                  >>^ toAbsRef base

getDocReferences                :: ArrowXml a => a XmlTree URI
getDocReferences                = fromLA (getRefs $< computeDocBase)
    where
    getRefs base                = multi selRefs >>^ toAbsRef base
        where
          hasLocName n          = hasNameWith ((== n) . localPart)
          selRef en an          = hasLocName en :-> getAttrValue0 an
          selRefs               = choiceA $
                                  map (uncurry selRef) names
                                  ++
                                  [ appletRefs
                                  , objectRefs
                                  , this :-> none
                                  ]
          names                 = [ ("img",     "src")
                                  , ("input",   "src")          -- input type="image" scr="..."
                                  , ("link",    "href")
                                  , ("script",  "src")
                                  ]                             
          appletRefs            = hasLocName "applet"   :-> (getAppRef $< getAppBase)
              where
                getAppBase      = (getAttrValue0 "codebase" `withDefault` ".") >>^ toAbsRef base
                getAppRef ab    = getAttrValue0 "code" >>^ toAbsRef ab

          objectRefs            = hasLocName "object"   :-> none        -- TODO

-- | construct an absolute URI by a base URI and a possibly relative URI

toAbsRef                        :: URI -> URI -> URI
toAbsRef base ref               = ( expandURIString ref                 -- here >>> is normal function composition
                                    >>>
                                    fromMaybe ref
                                    >>>
                                    removeFragment
                                  ) base
    where
    removeFragment r
        | "#" `isPrefixOf` path = reverse . tail $ path
        | otherwise             = r
        where
        path                    = dropWhile (/='#') . reverse $ r 

-- ------------------------------------------------------------

-- | Compute the base URI of a HTML page with respect to a possibly
--   given base element in the head element of a html page.
--
--   Stolen from Uwe Schmidt, http:\/\/www.haskell.org\/haskellwiki\/HXT
--   and then stolen back again by Uwe from Holumbus.Utility

computeDocBase                  :: ArrowXml a => a XmlTree String
computeDocBase                  = ( ( ( getByPath ["html", "head", "base"]
                                        >>>
                                        getAttrValue "href"                     -- and compute document base with transfer uri and base
                                      )
                                      &&&
                                      getAttrValue transferURI
                                    )
                                    >>> expandURI
                                  )
                                  `orElse`
                                  getAttrValue transferURI              -- the default: take the transfer uri

-- ------------------------------------------------------------

getByPath                       :: ArrowXml a => [String] -> a XmlTree XmlTree
getByPath                       = seqA . map (\ n -> getChildren >>> hasName n)

getHtmlTitle                    :: ArrowXml a => a XmlTree String
getHtmlTitle                    = getAllText $
                                  getByPath ["html", "head", "title"]

getHtmlPlainText                :: ArrowXml a => a XmlTree String
getHtmlPlainText                = getAllText $
                                  getByPath ["html", "body"]

getAllText                      :: ArrowXml a => a XmlTree XmlTree -> a XmlTree String
getAllText getText'             = ( getText'
                                    >>>
                                    ( fromLA $ deep getText )
                                    >>^
                                    (" " ++)                            -- text parts are separated by a space
                                  )
                                  >. (concat >>> normalizeWS)           -- normalize Space

isHtmlContents                  :: ArrowXml a => a XmlTree XmlTree
isHtmlContents                  = ( getAttrValue transferMimeType
                                    >>>
                                    isA ( `elem` [text_html, application_xhtml] )
                                  ) `guards` this

isPdfContents                   :: ArrowXml a => a XmlTree XmlTree
isPdfContents                   = ( getAttrValue transferMimeType
                                    >>>
                                    isA ( == application_pdf )
                                  ) `guards` this

getTitleOrDocName               :: ArrowXml a => a XmlTree String
getTitleOrDocName               = ( getHtmlTitle >>> isA (not . null) )
                                  `orElse`
                                  ( getAttrValue transferURI >>^ takeFileName )

isElemWithAttr                  :: ArrowXml a => String -> String -> (String -> Bool) -> a XmlTree XmlTree
isElemWithAttr en an av         = isElem
                                  >>>
                                  hasName en
                                  >>>
                                  hasAttrValue an av

-- ------------------------------------------------------------

application_pdf                 :: String
application_pdf                 = "application/pdf"

-- ------------------------------------------------------------

-- | normalize whitespace by splitting a text into words and joining this together with unwords

normalizeWS                     :: String -> String
normalizeWS                     = words >>> unwords

-- | take the first n chars of a string, if the input
--   is too long the cut off is indicated by \"...\" at the end

limitLength     :: Int -> String -> String
limitLength n s
    | length s' <= n            = s
    | otherwise                 = take (n - 3) s' ++ "..."
    where
    s'                          = take (n + 1) s

-- ------------------------------------------------------------
