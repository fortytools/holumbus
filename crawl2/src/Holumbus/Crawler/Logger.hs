{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Logger
    ( hxtLoggerName
    , hxtSetTraceAndErrorLogger
    , hxtSetLogLevel
    , hxtSetErrorLog

    , module System.Log.Logger

    , logC'
    , logC
    , noticeC
    , infoC
    , debugC
    , warnC
    , errC
    , setLogLevel
    )
where

import           Control.Monad.Trans

import 		 Data.List		( isPrefixOf )

import           Holumbus.Crawler.CrawlerAction

import           System.Log.Logger

import		 Text.XML.HXT.Arrow

crawlLoggerName	:: String
crawlLoggerName	= "crawl2"

hxtLoggerName	:: String
hxtLoggerName	= "hxt"

-- ------------------------------------------------------------

-- | Set trace level in config

logC	:: String -> Priority -> [String] -> CrawlerAction c r ()
logC logName' priority msg
     = liftIO $ logC' logName' priority msg

noticeC
  , infoC
  , debugC
  , warnC
  , errC	:: String -> [String] -> CrawlerAction c r ()
noticeC n	= logC n NOTICE
infoC   n	= logC n INFO
debugC  n       = logC n DEBUG
warnC   n	= logC n WARNING
errC    n       = logC n ERROR

setLogLevel	:: String -> Priority -> CrawlerAction c r ()
setLogLevel logName' priority
    = liftIO $ updateGlobalLogger (realLogName logName') (setLevel priority)

-- ------------------------------------------------------------

realLogName	:: String -> String
realLogName logName
    | null logName	= crawlLoggerName
    | otherwise	= crawlLoggerName ++ "." ++ logName


logC'	:: String -> Priority -> [String] -> IO ()
logC' logName' priority msg
    = logM logName priority msg'
    where
    logName 	= realLogName logName'
    logName16
	| length logName < 16	= logName ++ "\t"
	| otherwise		= logName
    msg'	= logName16 ++ "\t" ++ show priority ++ "\t" ++ unwords msg

-- ------------------------------------------------------------

hxtLogger :: Int -> String -> IO ()
hxtLogger level msg
    = logC' hxtLoggerName priority [msg']
    where
    msg'
	| "-- (" `isPrefixOf` msg	= drop 7 msg
	| otherwise			= msg
    priority = toPriority level
    toPriority l
	| l <= 0	= WARNING
	| l == 1    	= NOTICE
	| l == 2	= INFO
	| otherwise     = DEBUG			-- level >= 3
	           
hxtSetTraceAndErrorLogger	:: Priority -> IOStateArrow s b b
hxtSetTraceAndErrorLogger priority
    = hxtSetLogLevel priority
      >>>
      hxtSetErrorLog

hxtSetLogLevel			:: Priority -> IOStateArrow s b b
hxtSetLogLevel priority
    = setTraceLevel (fromPriority priority)
      >>>
      setTraceCmd hxtLogger
      >>>
      perform ( arrIO0 $
		updateGlobalLogger hxtLoggerName (setLevel priority)
	      )
    where
    fromPriority NOTICE	 = 1
    fromPriority INFO	 = 2
    fromPriority DEBUG	 = 3
    fromPriority _	 = 0

hxtSetErrorLog	:: IOStateArrow s b b
hxtSetErrorLog	= setErrorMsgHandler False hxtErrorLogger

hxtErrorLogger	:: String -> IO ()
hxtErrorLogger msg
    = logC' hxtLoggerName priority [drop 1 . dropWhile (/= ':') $ msg]
    where
    priority = prio . drop 1 $ msg
    prio m
	| "fatal"   `isPrefixOf` m	= CRITICAL
        | "error"   `isPrefixOf` m	= ERROR
	| "warning" `isPrefixOf` m	= WARNING
	| otherwise			= NOTICE

-- ------------------------------------------------------------
