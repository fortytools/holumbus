-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Utility
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Small utility functions which are probably useful somewhere else, too.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Utility where

import Data.Char

import           Numeric
import           Holumbus.Index.Common
import qualified Holumbus.Index.Documents as DOC

import qualified Data.IntMap as IM
import qualified Data.List as L

import           Data.Maybe
import           Text.XML.HXT.Arrow
import           Text.Regex

-- | Split a string into seperate strings at a specific character sequence.
split :: Eq a => [a] -> [a] -> [[a]]
split _ []       = [[]] 
split at w@(x:xs) = maybe ((x:r):rs) ((:) [] . split at) (L.stripPrefix at w)
                    where (r:rs) = split at xs
 
-- | Join with a seperating character sequence.
join :: Eq a => [a] -> [[a]] -> [a]
join = L.intercalate

-- | Removes leading and trailing whitespace from a string.
strip :: String -> String
strip = stripWith isSpace

-- | Strip leading and trailing elements matching a predicate.
stripWith :: (a -> Bool) -> [a] -> [a]
stripWith f = reverse . dropWhile f . reverse . dropWhile f

-- | Escapes non-alphanumeric or space characters in a String
escape :: String -> String 
escape []     = []
escape (c:cs)
  = if isAlphaNum c || isSpace c 
      then c : escape cs
      else '%' : showHex (fromEnum c) "" ++ escape cs
      
{- | 
     Computes a filename for a local temporary file.
     Since filename computation might depend on the DocId it is also submitted
     as a parameter
-}
tmpFile :: DocId -> URI -> String
tmpFile _ u = escape u

{- | 
     Helper function to replace original URIs by the corresponding pathes for 
     the locally dumped files
-}     
tmpDocs :: String -> DOC.Documents a -> DOC.Documents a
tmpDocs tmpPath =  
    DOC.fromMap 
  . (IM.mapWithKey (\docId doc -> Document (title doc) (tmpPath ++ (tmpFile docId (uri doc))) Nothing))
  . DOC.toMap         
      

{- | Create Crawl filters based on regular expressions. The first Parameter defines the default 
     value if none of the supplied rules matches. The rule list is computed from the first element
     to the last. The first rule that matches the URI is applied. 
     
     example:
     
     > crawlFilter False [ ("/a/b/z", True )
     >                   , ("/a/b"  , False)
     >                   , ("/a"    , True )

     The default value for the filter is False like it will be in most cases unless you are trying
     to use Holumbus to build a google replacement. If you read the rules from bottom to top, all
     documents in "/a" will be included (which should be a single domain or ip address or maybe a
     set of these). The second rule disables the directory "/a/b" but with the first rule, the 
     subdirectory z is included again and "/a/b/a" to "/a/b/y" are excluded. Even though this could
     also be done with the 'simpleCrawlFilter', this saves you a lot of unnecessary code.
-} 
crawlFilter :: Bool -> [(String, Bool)] -> (URI -> Bool)
crawlFilter theDefault [] _ = theDefault
crawlFilter theDefault ((expr, b):expressions) theUri = 
  if isJust $ matchRegex (mkRegex expr) theUri then b else crawlFilter theDefault expressions theUri

-- | Create Crawl filters based on regular expressions. The first list defines 
--   regular expression of URIs that have to be crawled. Any new URI must match at least one of 
--   these regular expressions to be crawled. The second list consists of regular expressions for
--   pages that must not be crawled. This can be used to limit the set of documents defined by 
--   the including expressions. 
simpleCrawlFilter :: [String] -> [String] -> (URI -> Bool)
simpleCrawlFilter as ds theUri = isAllowed && (not isForbidden ) 
         where
         isAllowed   = foldl (&&) True  (map (matches theUri) as)
         isForbidden = foldl (||) False (map (matches theUri) ds)
         matches u a = isJust $ matchRegex (mkRegex a) u      
      
-- | some standard options for the readDocument function
standardReadDocumentAttributes :: [(String, String)]
standardReadDocumentAttributes = []
  ++ [ (a_parse_html, v_1)]
  ++ [ (a_issue_warnings, v_0)]
  ++ [ (a_tagsoup, v_1) ]
  ++ [ (a_use_curl, v_1)]
  ++ [ (a_options_curl, "-L")] --"--user-agent HolumBot/0.1 --location")]     
  ++ [ (a_encoding, isoLatin1)]      
  



-- | Comput the base of a webpage
--   stolen from Uwe Schmidt, http://www.haskell.org/haskellwiki/HXT  
computeDocBase  :: IOStateArrow s XmlTree String
computeDocBase
    = (     (     xshow (getXPathTrees "/html/head/base@href")
              &&& getBaseURI
            )
        >>> expandURI
      )
      `orElse` getBaseURI

  