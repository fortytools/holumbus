-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Data.Crunch
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT
  
  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
  This module provides compression for streams of 32-bit words. Because of 
  some internal restriction in GHC, which makes all fixed integer size equal
  in terms of bit-width, the algorithm tries to crunch as much numbers as
  possible into a single 64-bit word.

  Based on the Simple9 encoding scheme from this article:
  
    * Vo N. Anh, Alstair Moffat,
      \"/Inverted Index Compression Using Word-Aligned Binary Codes/\",
      Information Retrieval, 8 (1), 2005, pages 151-166

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults #-}

module Holumbus.Data.Crunch 
  (
  -- * Compression
  crunch8
  , crunch16
  , crunch32
  , crunch64
  
  -- * Decompression
  , decrunch8
  , decrunch16
  , decrunch32
  , decrunch64
  )
where

import Data.Bits
import Data.Word

data Width = W01 | W02 | W03 | W04 | W05 | W06 | W07 | W08 | W10 | W12 | W15 | W20 | W30 | W60
             deriving (Show, Eq, Enum)

-- | Increase performance by avoiding the calculation of maximum values.
value :: Width -> Word64
value W01 = (2 ^ 1) - 1
value W02 = (2 ^ 2) - 1
value W03 = (2 ^ 3) - 1
value W04 = (2 ^ 4) - 1
value W05 = (2 ^ 5) - 1
value W06 = (2 ^ 6) - 1
value W07 = (2 ^ 7) - 1
value W08 = (2 ^ 8) - 1
value W10 = (2 ^ 10) - 1
value W12 = (2 ^ 12) - 1
value W15 = (2 ^ 15) - 1
value W20 = (2 ^ 20) - 1
value W30 = (2 ^ 30) - 1
value W60 = (2 ^ 60) - 1

-- | Increase performance by avoiding the calculation of counts.
count :: Width -> Int
count W01 = 60
count W02 = 30
count W03 = 20
count W04 = 15
count W05 = 12
count W06 = 10
count W07 = 8
count W08 = 7
count W10 = 6
count W12 = 5
count W15 = 4
count W20 = 3
count W30 = 2
count W60 = 1

width :: Width -> Int
width W01 = 1
width W02 = 2
width W03 = 3
width W04 = 4
width W05 = 5
width W06 = 6
width W07 = 7
width W08 = 8
width W10 = 10
width W12 = 12
width W15 = 15
width W20 = 20
width W30 = 30
width W60 = 60

-- | Crunch some values by encoding several values into one 'Word64'. The values may not exceed
-- the upper limit of @(2 ^ 60) - 1@. This precondition is not checked! The compression works
-- best on small values, therefore a difference encoding (like the one in 
-- "Holumbus.Data.DiffList") prior to compression pays off well.
crunch64 :: [Word64] -> [Word64]
crunch64 [] = []
crunch64 s = crunch' s (count W01) W01 []

-- | Crunch a non empty list. This precondition is not checked!
crunch' :: [Word64] -> Int -> Width -> [Word64] -> [Word64]
crunch' [] 0 m r = [encode m r]
crunch' [] _ m r = crunch' r (count (succ m)) (succ m) []
crunch' s 0 m r = (encode m r):(crunch' s (count W01) W01 [])
crunch' s@(x:xs) n m r = if x <= value m then crunch' xs (n - 1) m (r ++ [x])
                         else crunch' (r ++ s) (count (succ m)) (succ m) []

-- | Encode some numbers with a given width. The number of elements in the list may not exceed 
-- the amount of numbers allowed for this width and the numbers in the list may not exceed the
-- upper bound for this width. These preconditions are not checked!
encode :: Width -> [Word64] -> Word64                         
encode w [] = rotateR (fromIntegral (fromEnum w)) (64 - (width w * count w))
encode w (x:xs) = rotateR (encode w xs .|. fromIntegral x) (width w)

-- | Decrunch a list of crunched values. No checking for properly encoded values is done, weird
-- results have to be expected if calling this function on a list of arbitrary values.
decrunch64 :: [Word64] -> [Word64]
decrunch64 [] = []
decrunch64 (x:xs) = (decode (width w) (count w) (value w) (rotateL x (width w))) ++ (decrunch64 xs)
                   where
                     w = toEnum $ fromIntegral (x .&. 15)  -- Extract the 4 selector bits.

-- Decode some numbers with a given width.
decode :: Int -> Int -> Word64 -> Word64 -> [Word64]
decode _ 0 _ _ = []
decode w n m x = (x .&. m):(decode w (n - 1) m (rotateL x w))

-- | Crunching 'Word8' values, defined in terms of 'crunch64'.
crunch8 :: [Word8] -> [Word64]
crunch8 = crunch64 . (map fromIntegral)

-- | Crunching 'Word16' values, defined in terms of 'crunch64'.
crunch16 :: [Word16] -> [Word64]
crunch16 = crunch64 . (map fromIntegral)

-- | Crunching 'Word32' values, defined in terms of 'crunch64'.
crunch32 :: [Word32] -> [Word64]
crunch32 = crunch64 . (map fromIntegral)

-- | Decrunching to 'Word8' values, defined in terms of 'decrunch64'.
decrunch8 :: [Word64] -> [Word8]
decrunch8 = (map fromIntegral) . decrunch64

-- | Decrunching to 'Word16' values, defined in terms of 'decrunch64'.
decrunch16 :: [Word64] -> [Word16]
decrunch16 = (map fromIntegral) . decrunch64

-- | Decrunching to 'Word32' values, defined in terms of 'decrunch64'.
decrunch32 :: [Word64] -> [Word32]
decrunch32 = (map fromIntegral) . decrunch64
