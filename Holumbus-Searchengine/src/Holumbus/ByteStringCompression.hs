module Holumbus.ByteStringCompression
where

import qualified Codec.Compression.BZip as BZ

import qualified Data.ByteString.Lazy   as BL
import           Data.Int

import           Debug.Trace            (trace)

-- ------------------------------------------------------------
--
-- select the default compression/decompression method
--
-- compressBZip               is pure bzip compression
-- compressBZipWithTrace      traces the compression process
-- compressBZipSmart          does not compress short ByteStrings <= 80 bytes
-- compressBZipSmartWithTrace traces the compression process

compress :: BL.ByteString -> BL.ByteString
-- compress = compressBZip
-- compress = compressBZipWithTrace
-- compress = compressBZipSmart
compress = compressBZipSmartWithTrace

decompress :: BL.ByteString -> BL.ByteString
-- decompress = decompressBZip
decompress = decompressBZipSmart

-- ------------------------------------------------------------

compressBZip :: BL.ByteString -> BL.ByteString
compressBZip = BZ.compress

compressBZipWithTrace :: BL.ByteString -> BL.ByteString
compressBZipWithTrace = traceCompress compressBZip

decompressBZip :: BL.ByteString -> BL.ByteString
decompressBZip = BZ.decompress

-- ------------------------------------------------------------

compressBZipSmart :: BL.ByteString -> BL.ByteString
compressBZipSmart = compressCond 80 compressBZip

compressBZipSmartWithTrace :: BL.ByteString -> BL.ByteString
compressBZipSmartWithTrace = traceCompress compressBZipSmart

decompressBZipSmart :: BL.ByteString -> BL.ByteString
decompressBZipSmart = decompressCond decompressBZip

{-# INLINE compressBZipSmart #-}
{-# INLINE decompressBZipSmart #-}

-- ------------------------------------------------------------

compressCond :: Int64 -> (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
compressCond lowerBound cf x
    | BL.length x <= lowerBound
        = BL.cons 0 x
    | otherwise
        = BL.cons 1 $ cf x

decompressCond :: (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
decompressCond df x
    | BL.head x == 0
        = BL.tail x
    | otherwise
        = df $ BL.tail x

{-# INLINE compressCond #-}
{-# INLINE decompressCond #-}

-- ------------------------------------------------------------

traceCompress :: (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
traceCompress f x
    = trace msg y
    where
      lin  = BL.length x
      y    = f x
      lout = BL.length y
      tod  = fromInteger . fromIntegral
      cp   = (tod lout / tod lin) :: Double
      msg  = "traceCompress: " ++ show (lin, lout) ++ ", factor = " ++ show cp

-- ------------------------------------------------------------
