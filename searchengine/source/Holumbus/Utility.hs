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


import           Control.Exception (bracket)

import           Data.Binary
import qualified Data.ByteString.Lazy   as B
import           Data.ByteString.Lazy.Char8     (pack)
import           Data.Char
import           Data.Digest.Pure.MD5
import qualified Data.IntMap            as IM
import qualified Data.List              as L
-- import           Data.Maybe

import           Holumbus.Index.Common
-- import qualified Holumbus.Index.Documents as DOC

import           Numeric

import           System.IO

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

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

-- | Removes leading whitespace from a string.
stripl :: String -> String
stripl = dropWhile isSpace

-- | Removes trailing whitespace from a string.
stripr :: String -> String
stripr = reverse . dropWhile isSpace . reverse

-- | Strip leading and trailing elements matching a predicate.
stripWith :: (a -> Bool) -> [a] -> [a]
stripWith f = reverse . dropWhile f . reverse . dropWhile f

-- | found on the haskell cafe mailing list
--   (<http://www.haskell.org/pipermail/haskell-cafe/2008-April/041970.html>).
--   Depends on bytestring >= 0.9.0.4 (?) 
strictDecodeFile :: Binary a => FilePath -> IO a
strictDecodeFile f  =
    bracket (openBinaryFile f ReadMode) hClose $ \h -> do
                                                       c <- B.hGetContents h
                                                       return $! decode c  

-- | partition the list of input data into a list of input data lists of
--   approximately the same specified length
partitionListByLength :: Int -> [a] -> [[a]]
partitionListByLength _ [] = []
partitionListByLength count l  = [take count l] ++ (partitionListByLength count (drop count l)) 

-- | partition the list of input data into a list of a specified number of input data lists with 
--   approximately the same length
partitionListByCount :: Int -> [a] -> [[a]]
partitionListByCount sublistCount list = partition sublistCount list
  where
  partition 0 _ = []
  partition sublists l 
    = let next = ((length l) `div` sublists)
      in  if next == 0  then [l]
                        else [take next l] ++ partition (sublists -1) (drop next l)

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
tmpFile _ u = let f = escape u
              in if (length f) > 255
                   then (show . md5 . pack) f
                   else f
-- | Helper function to replace original URIs by the corresponding pathes for 
--   the locally saved files     
tmpDocs :: HolDocuments d a => String -> d a -> d a
tmpDocs tmpPath =  
    fromMap 
  . (IM.mapWithKey (\docId doc -> Document (title doc) (tmpPath ++ (tmpFile docId (uri doc))) Nothing))
  . toMap         
      

-- | Compute the base of a webpage
--   stolen from Uwe Schmidt, http:\/\/www.haskell.org\/haskellwiki\/HXT
computeDocBase  :: ArrowXml a => a XmlTree String
computeDocBase
    = ( ( ( this
            /> hasName "html"
            /> hasName "head"
            /> hasName "base"
            >>> getAttrValue "href"
          )
          &&&
          getAttrValue "transfer-URI"
        )
        >>> expandURI
      )
      `orElse`
      getAttrValue "transfer-URI"  
      
traceOffset :: Int
traceOffset = 3

trcMsg          :: String -> IO ()
trcMsg m         = hPutStrLn stderr ('-':"- (0) " ++ m)

-- ------------------------------------------------------------
--
-- simple and usefull access arrows

getByPath	:: ArrowXml a => [String] -> a XmlTree XmlTree
getByPath	= seqA . map (\ n -> getChildren >>> hasName n)

robotsNo	:: String -> LA XmlTree XmlTree
robotsNo what	= none
		  `when`
		  ( getByPath ["html", "head", "meta"]
		    >>>
		    hasAttrValue "name" ( map toUpper
					  >>>
					  (== "ROBOTS")
					)
		    >>>
		    getAttrValue0 "content"
		    >>>
		    isA ( map (toUpper >>> (\ x -> if isLetter x then x else ' '))
			  >>>
			  words
			  >>>
			  (what `elem`)
			)
		  )	  

robotsNoIndex	:: ArrowXml a => a XmlTree XmlTree
robotsNoIndex	= fromLA $ robotsNo "NOINDEX"

robotsNoFollow	:: ArrowXml a => a XmlTree XmlTree
robotsNoFollow	= fromLA $ robotsNo "NOFOLLOW"

-- ------------------------------------------------------------
			  
