{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Trace
    ( traceCrawl )
where

import           Control.Concurrent
import 		 Control.Monad.Reader

import           Holumbus.Crawler.CrawlerAction
import           Holumbus.Crawler.Types

import           System.IO
import           System.IO.Unsafe

-- ------------------------------------------------------------

traceCrawl			:: Int -> [String] -> CrawlerAction c r ()
traceCrawl l msg		= do
				  l0 <- getConf theTraceLevel
				  when ( l <= l0 )
                                       ( liftIO $ logg $ unwords msg )

-- ------------------------------------------------------------
--
-- just for syncing log messages

stdErrSem		:: QSem
stdErrSem		= unsafePerformIO $ newQSem 1

waitStderr, signalStderr :: IO ()
waitStderr		= waitQSem stdErrSem
signalStderr		= signalQSem stdErrSem

logg			:: String -> IO ()
logg msg		= do
			  waitStderr
			  tid <- myThreadId
			  hPutStrLn stderr $ show tid ++ "\t : " ++ msg
			  hFlush    stderr
			  signalStderr

-- ------------------------------------------------------------

