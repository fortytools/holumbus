
module Holumbus.Data.Crunch 
  (
  -- * Compression
  crunch
  
  -- * Decompression
  , decrunch
  )
where

import Data.Bits
import Data.Word

crunch :: [Word8] -> [Word64]
crunch []     = []
crunch (x:xs) = crunch' xs 1 (shiftL (fromIntegral x) 8)

crunch' :: [Word8] -> Word8 -> Word64 -> [Word64]
crunch' [] n r     = [r .|. fromIntegral n]
crunch' (x:xs) n r = if n < 7 then crunch' xs (n + 1) (shiftL (r .|. fromIntegral x) 8)
                     else (crunch' xs 1 (shiftL (fromIntegral x) 8)) ++ [r .|. fromIntegral n]

decrunch :: [Word64] -> [Word8]
decrunch [] = []
decrunch (x:xs) = decrunch' xs (fromIntegral x) (shiftR x 8)

decrunch' :: [Word64] -> Word8 -> Word64 -> [Word8]
decrunch' [] 0 _ = []
decrunch' (x:xs) 0 _ = decrunch' xs (fromIntegral x) (shiftR x 8)
decrunch' xs n r = (decrunch' xs (n - 1) (shiftR r 8)) ++ [fromIntegral r]
