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

import qualified Data.List as L
import qualified Data.Map as M

-- | Normalizes a Haskell signature, e.g. @String -> Int -> Int@ will be transformed to 
-- @a->b->b@. All whitespace will be removed from the resulting string.
normalizeSignature :: String -> String
normalizeSignature = join "->" . (replaceTypes M.empty ['a'..'z']) . split "->" . filter (not . isSpace)
  where
  replaceTypes _ _ [] = []
  replaceTypes v t (x:xs) = let (nv, ut, rx) = replace in rx:(replaceTypes nv ut xs)
    where
    replace = let ut = [head t] in maybe (M.insert x ut v, tail t, ut) (\n -> (v, t, n)) (M.lookup x v)

-- | Strip unneeded whitespace from a signature, e.g. @String -> Map k a -> Int@ will be transformed
-- to @String->Map k a->Int@.
stripSignature :: String -> String
stripSignature = join "->" . map strip . split "->"

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
