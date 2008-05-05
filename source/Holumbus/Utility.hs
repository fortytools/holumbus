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
      
-- | Computes a filename for a local temporary file.
--   Since filename computation might depend on the DocId it is also submitted
--   as a parameter
tmpFile :: DocId -> URI -> String
tmpFile _ u = escape u

-- | Helper function to replace original URIs by the corresponding pathes for 
--   the locally dumped files
--   <br/>TODO: Move this to a module
tmpDocs :: String -> DOC.Documents a -> DOC.Documents a
tmpDocs tmpPath =  
    DOC.fromMap 
  . (IM.mapWithKey (\docId doc -> Document (title doc) (tmpPath ++ (tmpFile docId (uri doc))) Nothing))
  . DOC.toMap         
      
-- | Helper function to create Crawl filters based on regular expressions.
--   A excluding regular expression is always stronger than a including one.
mkCrawlFilter :: [String] -> [String] -> (URI -> Bool)
mkCrawlFilter as ds theUri = isAllowed && (not isForbidden ) 
         where
         isAllowed   = foldl (&&) True  (map (doesMatch theUri) as)
         isForbidden = foldl (||) False (map (doesMatch theUri) ds)
         doesMatch u a = isJust $ matchRegex (mkRegex $ a) u      
      