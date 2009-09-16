{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
( main )
where

import Holumbus.Crawler.Core
import System.IO
import System.Environment

--
import Holumbus.Crawler.IndexerCore
import Control.Monad (foldM)
import Holumbus.Index.Common    hiding ( URI )
import Examples2.Re.CommonStandalone
--

main	:: IO ()
main  = do
  ( base : f : n : [] ) <- getArgs
  let action	= crawlDocs [base]
  -- crawls all sites and makes plaindocs out of em
  (_, state) <- runCrawler action (crawlerConfig (maybeStringlist f follow,maybeStringlist n nofollow) (-1)) crawlerInitState
  -- putStrLn . show $ state
  let docs = cs_resultAccu state
  (IndexerState {ixs_index=idx, ixs_documents=docs'} ) <- foldM (\n p -> insertRawDoc p n) (emptyIndex) docs
  writeToXmlFile ( "index.xml") idx
  writeToBinFile( "index.bin") idx
  writeToBinFile( "docs.bin") docs'  
  writeToXmlFile ( "docs.xml") docs'

follow :: [String]
follow = [ "http://www.fh-wedel.de/.*" ]

nofollow :: [String]
nofollow =
  [
    ".*(pdf|PDF|jpg|gif|png|tar.gz|tgz|ppt|exe|txt|zip|doc|dot|ps|gz|nb|swf|JPG|tex|rss|mpg|mp3|m3u|java|svg|mdb|xls|docx|xslx|pptx|dta|lst|rar|avi|mp4)"
  , ".*/hdoc.*"
  , ".*/javadoc/.*"
  , ".*/java/.*/doc.*"
  , ".*/fileadmin/.*"
  , ".*/vorlesungen/c/beispiele/.*"
  , ".*/TclTk/program1.html"
  , ".*/~splan/.*"
  , ".*/~wk/.*"
  , ".*/~si/projekte/.*"
  , ".*/archiv/.*"
  , ".*/src/.*"
  , ".*/news/.*"
  , ".*/\\?L=.*"
  ]

maybeStringlist :: String -> [String] -> [String]
maybeStringlist s l
  | null s = l
  | otherwise = words s
