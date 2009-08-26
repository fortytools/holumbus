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
  ( base : [] ) <- getArgs
  let action	= crawlDocs [base]
  -- crawls all sites and makes plaindocs out of em
  (_, state) <- runCrawler action (crawlerConfig (base++".*") 500) crawlerInitState
  putStrLn . show $ state
  let docs = cs_resultAccu state
  (IndexerState {ixs_index=idx, ixs_documents=docs'} ) <- foldM (\n p -> insertRawDoc p n) (emptyIndex) docs
  writeToXmlFile ( "index.xml") idx
  writeToBinFile( "index.bin") idx
  writeToBinFile( "docs.bin") docs'  
  writeToXmlFile ( "docs.xml") docs'