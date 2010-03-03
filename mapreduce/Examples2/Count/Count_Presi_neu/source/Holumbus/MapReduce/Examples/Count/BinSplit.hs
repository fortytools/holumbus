module Holumbus.MapReduce.Examples.Count.BinSplit (binsplit) where

import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Int

-- | Split the given bytestring into ~n bytes. The bytestring is split at whole words with a space as separator
binsplit :: Int64 -> C.ByteString -> (C.ByteString,C.ByteString)
binsplit n bs = (part1',part2')
  where
  -- reassamble part1
  part1' = C.append part1 restOfPart1

  -- get the rest from part2 belonging to part1
  (restOfPart1,part2') = C.span (/= ' ') part2

  -- split at the number of bytes
  (part1,part2) = C.splitAt n bs

