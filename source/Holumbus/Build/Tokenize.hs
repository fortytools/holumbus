-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Build.Crawl
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT
  
  Maintainer : Sebastian M. Schlatt (sms@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module provides functions to split Strings into word lists.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Build.Tokenize where

import Data.Char

parseWords  :: (Char -> Bool) -> String -> [String]
parseWords isWordChar'
          = filter (not . null) . words . map boringChar
          where
          boringChar c             -- these chars separate words
            | isWordChar' c = c
            | otherwise    = ' '

isWordChar  :: Char -> Bool
isWordChar c = isAlphaNum c || c `elem` ".-_'@"  