-- ----------------------------------------------------------------------------
{- |
  Module     : 
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Main(main) where

import           Holumbus.Common.FileHandling
import           Holumbus.MapReduce.Demo
import           Holumbus.MapReduce.Types

version :: String
version = "Pickle-Test 0.1"

main :: IO ()
main
  = do
    putStrLn version
    putStrLn "writing: job.xml"
    saveToXmlFile "job.xml" demoJob
    putStrLn "reading: job.xml"
    ji <- (loadFromXmlFile "job.xml")::IO JobInfo
    putStrLn $ show ji
    putStrLn $ "comparing: " ++ show (ji == demoJob)