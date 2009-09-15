{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.PdfToText
where

-- import		 Control.Exception

import		 System.Process.Pipe		( pipeString )
import 		 System.Directory 		( getTemporaryDirectory
						, removeFile
						)
import		 System.IO
import		 Text.XML.HXT.DOM.Unicode	( utf8ToUnicode )
import		 Text.XML.HXT.Arrow

-- ------------------------------------------------------------

pdfToText	:: String -> IO String
pdfToText inp	= ( do
		    td <- getTemporaryDirectory
		    (fn, h) <- openTempFile td "indexer.pdf"
		    hPutStr h inp
		    hFlush h
		    hClose h
		    res <- pipeString [( "pdftotext"
				       , ["-q", "-enc", "UTF-8", fn, "-"]
				       )] ""
		    removeFile fn
		    return ( fst . utf8ToUnicode $ res )
		  ) `catch` ( const $ return "" )

pdfToTextA	:: IOSArrow String String
pdfToTextA	= perform ( traceString 2 (("pdfToTextA input:\n" ++) . (take 1024)) )
		  >>>
		  arrIO pdfToText
		  >>>
		  perform ( traceString 2 ( "pdfToText result:\n" ++ ) )

-- ------------------------------------------------------------


