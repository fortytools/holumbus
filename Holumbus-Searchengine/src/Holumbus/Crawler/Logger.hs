{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.Logger
    ( hxtLoggerName
    , hxtSetTraceAndErrorLogger
    , hxtSetLogLevel
    , hxtSetErrorLog

    , module System.Log.Logger

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

import           Data.List              ( isPrefixOf )

import           System.Log.Logger

import           Text.XML.HXT.Core

crawlLoggerName                 :: String
crawlLoggerName                 = "crawl2"

hxtLoggerName                   :: String
hxtLoggerName                   = "hxt"

-- ------------------------------------------------------------

-- | Set trace level in config

logC                            :: MonadIO m => String -> Priority -> [String] -> m ()
logC logName' priority msg      = liftIO $ logC' logName' priority msg

noticeC
  , infoC
  , debugC
  , warnC
  , errC                        :: MonadIO m => String -> [String] -> m ()

noticeC n                       = logC n NOTICE
infoC   n                       = logC n INFO
debugC  n                       = logC n DEBUG
warnC   n                       = logC n WARNING
errC    n                       = logC n ERROR

setLogLevel                     :: MonadIO m => String -> Priority -> m ()
setLogLevel  logName' priority  = liftIO $ setLogLevel' logName' priority

setLogLevel'                    :: String -> Priority -> IO ()
setLogLevel' logName' priority  = updateGlobalLogger (realLogName logName') (setLevel priority)

-- ------------------------------------------------------------

realLogName                     :: String -> String
realLogName logName
    | null logName              = crawlLoggerName
    | otherwise                 = crawlLoggerName ++ "." ++ logName


logC'                           :: String -> Priority -> [String] -> IO ()
logC' logName' priority msg     = logM logName priority msg'
    where
    logName                     = realLogName logName'
    msg'                        = fillName 23 logName        ++ " " ++
                                  fillName 9 (show priority) ++ " " ++
                                  unwords msg

fillName                        :: Int -> String -> String
fillName n s                    = s ++ replicate b ' '
    where
    b                           = (n - length s) `max` 0

-- ------------------------------------------------------------

hxtLogger                       :: Int -> String -> IO ()
hxtLogger level msg             = logC' hxtLoggerName priority [msg']
    where
    msg'
        | "-- (" `isPrefixOf` msg       = drop 7 msg
        | otherwise                     = msg

    priority = toPriority level

    toPriority l
        | l <= 0                = NOTICE                -- trace level 0 is issued as NOTICE, not as WARNING
        | l == 1                = NOTICE
        | l == 2                = INFO
        | otherwise             = DEBUG                 -- level >= 3
                   
hxtSetTraceAndErrorLogger       :: Priority -> IOStateArrow s b b
hxtSetTraceAndErrorLogger priority
                                = hxtSetLogLevel priority
                                  >>>
                                  hxtSetErrorLog

hxtSetLogLevel                  :: Priority -> IOStateArrow s b b
hxtSetLogLevel priority
                                = setTraceLevel (fromPriority priority)
                                  >>>
                                  setTraceCmd hxtLogger
                                  >>>
                                  perform ( arrIO0 $
                                            updateGlobalLogger hxtLoggerName (setLevel priority)
                                          )
    where
    fromPriority NOTICE         = 1
    fromPriority INFO           = 2
    fromPriority DEBUG          = 3
    fromPriority _              = 0

hxtSetErrorLog                  :: IOStateArrow s b b
hxtSetErrorLog                  = setErrorMsgHandler False hxtErrorLogger

hxtErrorLogger                  :: String -> IO ()
hxtErrorLogger msg              = logC' hxtLoggerName
                                        priority
                                        [drop 1 . dropWhile (/= ':') $ msg]
    where
    priority                    = prio . drop 1 $ msg
    prio m
        | "fatal"   `isPrefixOf` m      = CRITICAL
        | "error"   `isPrefixOf` m      = ERROR
        | "warning" `isPrefixOf` m      = WARNING
        | otherwise                     = NOTICE

-- ------------------------------------------------------------
