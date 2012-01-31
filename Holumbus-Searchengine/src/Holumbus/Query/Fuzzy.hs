-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Fuzzy
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

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
  , FuzzyConfig (..)
  
  -- * Predefined replacements
  , englishReplacements
  , germanReplacements
  
  -- * Generation
  , fuzz
  
  -- * Conversion
  , toList
  )
where

import Data.Binary
import Data.List
import Data.Function

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M

-- | A set of string which have been "fuzzed" with an associated score.

type FuzzySet = Map String FuzzyScore

-- | Some replacements which can be applied to a string to generate a 'FuzzySet'. The scores of
-- the replacements will be normalized to a maximum of 1.0.

type Replacements = [ Replacement ]

-- | A single replacements, where the first will be replaced by the second and vice versa in
-- the target string. The score indicates the amount of fuzzines that one single application
-- of this replacement in just one direction will cause on the target string.

type Replacement = ((String, String), FuzzyScore)

-- | The score indicating an amount of fuzziness. 

type FuzzyScore = Float

-- | The configuration of a fuzzy query.

data FuzzyConfig
    = FuzzyConfig 
      { applyReplacements  :: Bool         -- ^ Indicates whether the replacements should be applied.
      , applySwappings     :: Bool         -- ^ Indicates whether the swapping of adjacent characters should be applied.
      , maxFuzziness       :: FuzzyScore   -- ^ The maximum allowed fuzziness.
      , customReplacements :: Replacements -- ^ The replacements that should be applied.
      }
    deriving (Show)

instance Binary FuzzyConfig where
  put (FuzzyConfig r s m f)
      = put r >> put s >> put m >> put f
  get
      = liftM4 FuzzyConfig get get get get

-- | Some default replacements for the english language.
englishReplacements :: Replacements
englishReplacements =
  [ (("l", "ll"), 0.2)
  , (("t", "tt"), 0.2)
  , (("r", "rr"), 0.2)
  , (("e", "ee"), 0.2)
  , (("o", "oo"), 0.2)
  , (("s", "ss"), 0.2)
  
  , (("g", "ck"), 0.4)
  , (("k", "ck"), 0.4)
  , (("ea", "ee"), 0.4)
  , (("ou", "oo"), 0.4)
  , (("ou", "au"), 0.4)
  , (("ou", "ow"), 0.4)

  , (("s", "c"), 0.6)
  , (("uy", "ye"), 0.6)
  , (("y", "ey"), 0.6)
  , (("kn", "n"), 0.6)
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

-- | Continue fuzzing a string with the an explicitly specified list of replacements until 
-- a given score threshold is reached.

fuzz :: FuzzyConfig -> String -> FuzzySet
fuzz cfg s = M.delete s (fuzz' (fuzzLimit cfg 0.0 s))
  where
  fuzz' :: FuzzySet -> FuzzySet
  fuzz' fs = if M.null more then fs else M.unionWith min fs (fuzz' more)
    where
    -- The current score is doubled on every recursive call, because fuzziness increases exponentially.
    more = M.foldrWithKey (\sm sc res -> M.unionWith min res (fuzzLimit cfg (sc + sc) sm)) M.empty fs

-- | Fuzz a string and limit the allowed score to a given threshold.

fuzzLimit :: FuzzyConfig -> FuzzyScore -> String -> FuzzySet
fuzzLimit cfg sc s = if sc <= th then M.filter (\ns -> ns <= th) (fuzzInternal cfg sc s) else M.empty
  where
  th = maxFuzziness cfg

-- | Fuzz a string with an list of explicitly specified replacements and combine the scores
-- with an initial score.

fuzzInternal :: FuzzyConfig -> FuzzyScore -> String -> FuzzySet
fuzzInternal cfg sc s = M.unionWith min replaced swapped
  where
  replaced = let rs = customReplacements cfg in if (applyReplacements cfg) 
             then foldr (\r res -> M.unionWith min res (applyFuzz (replace rs r) sc s)) M.empty rs
             else M.empty
  swapped = if (applySwappings cfg) 
            then applyFuzz swap sc s
            else M.empty

-- | Applies a fuzzy function to a string. An initial score is combined with the new score 
-- for the replacement.

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
