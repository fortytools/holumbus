-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Common.FileHandling
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


  Some nice helper functions for strict file writing, appending and
  reading.

-}
-- ----------------------------------------------------------------------------

module Holumbus.Common.FileHandling
(
-- * xml files
  loadFromXmlFile
, saveToXmlFile

-- * lists in binary files
, writeToListFile
, appendToListFile
, readFromListFile
, parseByteStringToList

-- * bytestring file handling
, writeToBinFile
, appendToBinFile
, readFromBinFile

-- * string file handling
, writeToTextFile
, appendToTextFile
, readFromTextFile
)
where


import           Control.Exception
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import           Data.Char
import           Foreign
import           System.IO
import           System.IO.Unsafe

import           Text.XML.HXT.Arrow




-- ----------------------------------------------------------------------------
-- xml files
-- ----------------------------------------------------------------------------


loadFromXmlFile :: (XmlPickler a) => FilePath -> IO a
loadFromXmlFile f
  = do
    r <- runX (xunpickleDocument xpickle options f)
    return $! head r
    where
    options = [ (a_validate,v_0), (a_remove_whitespace, v_1), (a_encoding, utf8), (a_validate, v_0) ]


saveToXmlFile :: (XmlPickler a) => FilePath -> a -> IO ()
saveToXmlFile f i 
  = do
    runX (constA i >>> xpickleDocument xpickle options f)
    return ()
    where
    options = [ (a_indent, v_1), (a_output_encoding, utf8), (a_validate, v_0) ]  



-- ----------------------------------------------------------------------------
-- generic lists
-- ----------------------------------------------------------------------------


writeToListFile :: (Binary a) => FilePath -> [a] -> IO ()
writeToListFile fp bs = writeToBinFile fp $ B.concat $ map encode bs


appendToListFile :: (Binary a) => FilePath -> [a] -> IO ()
appendToListFile fp bs = appendToBinFile fp $ B.concat $ map encode bs

    
readFromListFile :: (Binary a) => FilePath -> IO [a]
readFromListFile f
   = do
     b <- readFromBinFile f 
     return $ parseByteStringToList b


parseByteStringToList :: (Binary a) => B.ByteString -> [a]
parseByteStringToList b = reverse $ parse b []
  where
  parse :: (Binary a) => B.ByteString -> [a] -> [a]
  parse bs accu
    | (B.null bs) = accu
    | otherwise   = parse (B.drop count bs) ([nextElem] ++ accu) 
    where
    nextElem = decode bs
    count    = B.length (encode nextElem)
     


-- ----------------------------------------------------------------------------     
-- strict functions, bytestrings only     
-- ----------------------------------------------------------------------------


writeToBinFile :: FilePath -> B.ByteString -> IO ()
writeToBinFile = B.writeFile

    
appendToBinFile :: FilePath -> B.ByteString -> IO ()
appendToBinFile = B.appendFile

    
readFromBinFile :: FilePath -> IO B.ByteString
readFromBinFile f  
   = bracket (openBinaryFile f ReadMode) hClose $ 
       \h -> do
       s <- hFileSize h
       c <- B.hGetNonBlocking h (fromInteger s)
       return $! c    


-- ----------------------------------------------------------------------------     
-- strict functions, strings only     
-- ----------------------------------------------------------------------------

     
writeToTextFile :: FilePath -> String -> IO ()
writeToTextFile = writeFile


appendToTextFile :: FilePath -> String -> IO ()
appendToTextFile = appendFile

-- strict file reading by Simon Marlow
-- found on
-- http:\/\/users.aber.ac.uk\/afc\/stricthaskell.html
readFromTextFile :: FilePath -> IO String
readFromTextFile f
  = do
    bracket (openFile f ReadMode) hClose $ 
      \h -> do
      s <- hFileSize h
      fp <- mallocForeignPtrBytes (fromIntegral s)
      len <- withForeignPtr fp $ \buf -> hGetBuf h buf (fromIntegral s)
      lazySlurp fp 0 len

buf_size :: Int
buf_size = 4096

lazySlurp :: ForeignPtr Word8 -> Int -> Int -> IO String
lazySlurp fp ix len
  | fp `seq` False = undefined
  | ix >= len = return []
  | otherwise = do
      cs <- unsafeInterleaveIO (lazySlurp fp (ix + buf_size) len)
      ws <- withForeignPtr fp $ \p -> loop' (min (len-ix) buf_size - 1) 
          ((p :: Ptr Word8) `plusPtr` ix) cs
      return ws
  where
  loop' :: Int -> Ptr Word8 -> String -> IO String
  loop' len' p acc
    | len' `seq` p `seq` False = undefined
    | len' < 0 = return acc
    | otherwise = do
       w <- peekElemOff p len'
       loop' (len'-1) p (chr (fromIntegral w):acc)