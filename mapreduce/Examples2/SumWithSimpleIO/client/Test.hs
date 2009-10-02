module Main where

import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put
import Data.Binary.Get

-- | Decode records in repetition
decodeStream :: Binary a => B.ByteString -> [a]
decodeStream = runGet (getStream get)

-- | Encode list of records as bytestring
encodeStream :: Binary a => [a] -> B.ByteString 
encodeStream = runPut . putStream put

-- | Read list of values from bytestring until it ends.
getStream :: Get a -> Get [a]
getStream getter = do
  empty <- isEmpty
  if empty
    then return []
    else do x <- getter
            xs <- getStream getter
            x `seq` return (x:xs)

-- | Write list of values.
putStream :: (a -> Put) -> [a] -> Put
putStream f = mapM_ f

main :: IO ()
main = encodeFile "test.bin" (encodeStream ([1..10^7]::[Integer]))
