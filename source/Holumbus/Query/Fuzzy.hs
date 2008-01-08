-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Fuzzy
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  The unique Holumbus mechanism for generating fuzzy sets.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Fuzzy 
  (
  -- * Fuzzy types
  FuzzySet
  , Replacements
  , Replacement
  , FuzzyScore
  
  -- * Predefined replacements
  , englishReplacements
  , germanReplacements
  
  -- * Generation
  , fuzz
  , fuzzWith
  , fuzzMore
  , fuzzMoreWith
  , fuzzUntil
  , fuzzUntilWith
  
  -- * Conversion
  , toList
  )
where

import Data.List

import Data.Map (Map)
import qualified Data.Map as M

-- | A set of string which have been "fuzzed" with an associated score.
type FuzzySet = Map String FuzzyScore
-- | Some replacements which can be applied to a string to generate a @FuzzySet@.
type Replacements = [ Replacement ]
-- | A single replacements, where the first will be replaced by the second and vice versa in
-- the target string. The score indicates the amount of fuzzines that one single application
-- of this replacement in just one direction will cause on the target string.
type Replacement = ((String, String), FuzzyScore)
-- | The score indicating an amount of fuzziness. 
type FuzzyScore = Float

-- | The default replacements to use in the functions without explicitly specified replacements.
defaultReplacements :: Replacements
defaultReplacements = germanReplacements

-- | Some default replacements for the english language.
englishReplacements :: Replacements
englishReplacements =
  [ (("l", "ll"), 0.2)
  , (("t", "tt"), 0.2)
  , (("r", "rr"), 0.2)
  ]

-- | Some default replacements for the german language.
germanReplacements :: Replacements
germanReplacements = 
  [ (("l", "ll"), 0.2)
  , (("t", "tt"), 0.2)
  , (("n", "nn"), 0.2)
  , (("r", "rr"), 0.2)
  , (("i", "ie"), 0.2)
  , (("ei", "ie"), 0.2)
  , (("k", "ck"), 0.2)

  , (("d", "t"), 0.4)
  , (("b", "p"), 0.4)
  , (("g", "k"), 0.4)
  , (("g", "ch"), 0.4)
  , (("c", "k"), 0.4)
  , (("s", "z"), 0.4)
  , (("u", "ou"), 0.4)

  , (("ü", "ue"), 0.1)
  , (("ä", "ae"), 0.1)
  , (("ö", "oe"), 0.1)
  , (("ß", "ss"), 0.1)
  ]

-- | Fuzz a string usind the default replacements.
fuzz :: String -> FuzzySet
fuzz = fuzzWith defaultReplacements

-- | Fuzz a string using an explicitly specified list of replacements.
fuzzWith :: Replacements -> String -> FuzzySet
fuzzWith = fuzzInternal 0.0

-- | Fuzz a set of fuzzy strings even more using the default replacements (the new set of 
-- fuzzy strings is not merged with the original set).
fuzzMore :: FuzzySet -> FuzzySet
fuzzMore = fuzzMoreWith defaultReplacements

-- | Fuzz a set of fuzzy strings even more using an explicitly specified list of replacements.
-- (the new set of fuzzy strings is not merged with the original set).
fuzzMoreWith :: Replacements -> FuzzySet -> FuzzySet
fuzzMoreWith rs fs = M.foldWithKey (\s sc res -> M.unionWith min res (fuzzInternal sc rs s)) M.empty fs

-- | Continue fuzzing a string with the default replacements until a given score threshold is reached.
fuzzUntil :: FuzzyScore -> String -> FuzzySet
fuzzUntil = fuzzUntilWith defaultReplacements

-- | Continue fuzzing a string with the an explicitly specified list of replacements until 
-- a given score threshold is reached.
fuzzUntilWith :: Replacements -> FuzzyScore -> String -> FuzzySet
fuzzUntilWith rs th s = fuzzUntilWith' (fuzzLimit th 0.0 rs s)
  where
  fuzzUntilWith' :: FuzzySet -> FuzzySet
  fuzzUntilWith' fs = if M.null more then fs else M.unionWith min fs (fuzzUntilWith' more)
    where
    -- The current score is doubled on every recursive call, because fuzziness increases exponentially.
    more = M.foldWithKey (\sm sc res -> M.unionWith min res (fuzzLimit th (sc + sc) rs sm)) M.empty fs

-- | Fuzz a string and limit the allowed score to a given threshold.
fuzzLimit :: FuzzyScore -> FuzzyScore -> Replacements -> String -> FuzzySet
fuzzLimit th sc rs s = if sc <= th then M.filter (\ns -> ns <= th) (fuzzInternal sc rs s) else M.empty

-- | Fuzz a string with an list of explicitly specified replacements and combine the scores
-- with an initial score.
fuzzInternal :: FuzzyScore -> Replacements -> String -> FuzzySet
fuzzInternal sc rs s = M.unionWith min replaced swapped
  where
  replaced = foldr (\r res -> M.unionWith min res (applyFuzz (replace rs r) sc s)) M.empty rs
  swapped = applyFuzz swap sc s

-- | Applies a single replacement definition (in both directions) to a string. An initial score is
-- combined with the new score for the replacement (calculated from the position in the string and 
-- the scores in the list of all replacements).
applyFuzz :: (String -> String -> [(String, FuzzyScore)]) -> FuzzyScore -> String -> FuzzySet
applyFuzz f sc s = apply (init $ inits s) (init $ tails s)
  where
  apply :: [String] -> [String] -> FuzzySet
  apply [] _ = M.empty
  apply _ [] = M.empty
  apply (pr:prs) (su:sus) = M.unionsWith min $ (apply prs sus):(map create $ (f pr su))
    where
    create (fuzzed, score) = M.singleton fuzzed (sc + score * (calcWeight (length pr) (length s)))

-- | Apply a replacement in both directions to the suffix of a string and return the complete
-- string with a score, calculated from the replacement itself and the list of replacements.
replace :: Replacements -> Replacement -> String -> String -> [(String, FuzzyScore)]
replace rs ((r1, r2), s) prefix suffix = (replace' r1 r2) ++ (replace' r2 r1)
  where
  replace' tok sub = if replaced == suffix then [] else [(prefix ++ replaced, score)]
    where
    replaced = replaceFirst tok sub suffix
    score = s / (snd $ maximumBy (compare `on` snd) rs)
    
-- | Swap the first two characters of the suffix and return the complete string with a score or
-- Nothing if there are not enough characters to swap.
swap :: String -> String -> [(String, FuzzyScore)]
swap prefix (s1:s2:suffix) =  [(prefix ++ (s2:s1:suffix), 1.0)]
swap _ _ = []

-- | Calculate the weighting factor depending on the position in the string and it's total length.
calcWeight :: Int -> Int -> FuzzyScore
calcWeight pos len = (l - p) / l
  where
  p = fromIntegral pos
  l = fromIntegral len

-- | Searches a prefix and replaces it with a substitute in a list.
replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst []       ys zs       = ys ++ zs
replaceFirst _        _ []       = []
replaceFirst t@(x:xs) ys s@(z:zs) = if x == z && t `isPrefixOf` s then 
                                      if null ys then replaceFirst xs [] zs 
                                      else (head ys) : replaceFirst xs (tail ys) zs
                                    else s

-- | Transform a fuzzy set into a list (ordered by score).
toList :: FuzzySet -> [ (String, FuzzyScore) ]
toList = sortBy (compare `on` snd) . M.toList

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y
