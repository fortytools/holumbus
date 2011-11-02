-- ----------------------------------------------------------------------------

{- |
  Module     : Helpers

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Some helpers that don't fit into other modules
-}

-- ----------------------------------------------------------------------------

module Helpers
  ( box
  , strToInt
  )
where

-- ----------------------------------------------------------------------------
-- | Make an one-item-List      
box :: a -> [a]
box x = [x]

------------------------------------------------------------------------------
-- | convert a String to an Int.
-- | returns defaultValue if conversion fails
strToInt :: Int -> String -> Int
strToInt defaultValue str
  | (length readsResult > 0) = fst $ head readsResult
  | otherwise = defaultValue
  where
  readsResult = reads $ str

