{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.PdfToText
where

import           Control.Concurrent.MVar
import qualified Control.Exception              as CE

import qualified Data.ByteString.Lazy           as BS
import           Data.String.Unicode            ( utf8ToUnicode )

import           System.Directory               ( getTemporaryDirectory
                                                , removeFile
                                                )
import           System.FilePath                ( (</>) )
import           System.Process                 ( rawSystem )
import           System.Posix.Process           ( getProcessID )
import           System.IO.Unsafe               ( unsafePerformIO )

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | Conversion of pdf data into plain text. The conversion is done
-- by calling an external program @pdftotext@ (contained in linux packages @xpdf@).
-- IO is done via the ByteString API.

pdfToText       :: String -> IO String
pdfToText       = pdfToTextBS . BS.pack . map (toEnum . fromEnum)

pdfToTextBS     :: BS.ByteString -> IO String
pdfToTextBS inp = ( do
                    fns@(fn1, fn2) <- requestPdf
                    BS.writeFile fn1 inp
                    _       <- rawSystem "pdftotext" ["-q", "-enc", "UTF-8", fn1, fn2]
                    removeFile fn1
                    res     <- BS.readFile fn2
                    BS.length res `seq`
                      removeFile fn2
                    releasePdf fns

                    return ( fst . utf8ToUnicode . map (toEnum . fromEnum) . BS.unpack $ res )
                  ) `mycatch` ( const $ return "" )
  where
  mycatch       :: IO a -> (CE.SomeException -> IO a) -> IO a
  mycatch       = CE.catch

pdfToTextA      :: IOSArrow String String
pdfToTextA      = perform ( traceString 2 (("pdfToTextA input:\n" ++) . take 128 . show) )
                  >>>
                  arrIO pdfToText
                  >>>
                  perform ( traceString 2 (( "pdfToText result:\n" ++ ) . take 128 . show) )

-- ------------------------------------------------------------

-- The pdftotext call is not thread save

pdfResource    :: MVar (FilePath, FilePath)
pdfResource
    = unsafePerformIO $ tmpFiles >>= newMVar
    where
      tmpFiles
          = do td      <- getTemporaryDirectory
               pid     <- getProcessID
               let fn1 = fn td pid "pdfToText.pdf"
               let fn2 = fn td pid "pdfToText.txt"
               return (fn1, fn2)
      fn d p f
          = d </> (show p ++ "-" ++ f)

{-# NOINLINE pdfResource #-}

requestPdf     :: IO (FilePath, FilePath)
requestPdf     = takeMVar pdfResource

releasePdf     :: (FilePath, FilePath) -> IO ()
releasePdf     = putMVar pdfResource

-- ------------------------------------------------------------
