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
  reading. All read-functions are strict, in the sense that
  the whole file is read at once and the content is stored into memory.
  So you won't have any semi-closed handles which might bring you in trouble
  from time to time. The files are immedialty closed after the reading.
  This Module can handle four filetypes: XML-Files, List-Files,
  Binary-Files and Text-Files.
  
  XML-Files:
  Normal textfiles, but the content is stored as a xml-representation. For
  the pickling, we use the HXT-Library 
  (see http:\/\/www.fh-wedel.de\/~si\/HXmlToolbox\/)
  
  List-Files:
  Binary-Files, which store a list of data-objects. The main difference
  between Binary-Files and List-Files is, that you can append data to a
  List-File which will be automatically concatenated to the existing list.
  If you try this with a normal Binary-File, you'll get only the original
  list and the appended data will be lost.
  
  Binary-Files:
  Normal binary files, for the encoding and decoding, we use the Haskell
  binary-package.
  
  Text-Files:
  Normal text files.

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
    , listToByteString

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
import           Data.List
import           Foreign
import           System.IO
import           System.IO.Unsafe

import           Text.XML.HXT.Arrow
import           Control.Parallel.Strategies

-- ----------------------------------------------------------------------------
-- xml files
-- ----------------------------------------------------------------------------


-- | Loads an XML-File and parses the content to a specified datatype.
loadFromXmlFile :: (XmlPickler a) => FilePath -> IO a
loadFromXmlFile f
  = do
    r <- runX (xunpickleDocument xpickle options f)
    return $! head r
    where
    options = [ (a_validate,v_0), (a_remove_whitespace, v_1), (a_encoding, utf8), (a_validate, v_0) ]


-- | Converts a dataset to XML and saves the XML-string into a file.
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


-- | Writes data to a list file.
writeToListFile :: (Binary a) => FilePath -> [a] -> IO ()
writeToListFile = encodeFile

-- | Appends data to a list file.
appendToListFile :: (Binary a) => FilePath -> [a] -> IO ()
appendToListFile fp = appendToBinFile fp . listToByteString

-- | reads from a list file.
readFromListFile :: (NFData a, Binary a) => FilePath -> IO [a]
readFromListFile = decodeFile

listToByteString :: (Binary a) => [a] -> B.ByteString
listToByteString = encode

parseByteStringToList :: (Binary a) => B.ByteString -> [a]
parseByteStringToList = decode

-- ----------------------------------------------------------------------------     
-- strict functions, bytestrings only     
-- ----------------------------------------------------------------------------

-- | Write data to a binary file.
writeToBinFile :: FilePath -> B.ByteString -> IO ()
writeToBinFile = B.writeFile


-- | Append data to a binary file, this doesn't mean, that the contents
--   are really concatenated when you read the file.    
appendToBinFile :: FilePath -> B.ByteString -> IO ()
appendToBinFile = B.appendFile


-- | Reads the data from a binary file as a bytestring.
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

     
-- | Writes a sting to a text file.
writeToTextFile :: FilePath -> String -> IO ()
writeToTextFile = writeFile

-- | Appends a string to a text file.
appendToTextFile :: FilePath -> String -> IO ()
appendToTextFile = appendFile


-- | Strict file reading by Simon Marlow.
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
       
       
 
