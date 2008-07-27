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

import           Control.Exception

import           Holumbus.Common.Logging
import qualified Holumbus.FileSystem.FileSystem as FS
import qualified Holumbus.FileSystem.UserInterface as UI


version :: String
version = "Holumbus-FileSystem Standalone-Client 0.1"


main :: IO ()
main
  = do
    putStrLn version
    handle (\e -> putStrLn $ "EXCEPTION: " ++ show e) $
      do
      initializeLogging      
      fs <- initializeData
      UI.runUI fs version      
      deinitializeData fs


initializeData :: IO (FS.FileSystem)
initializeData 
  = do
    fs <- FS.mkFileSystemClient "controller.port"
    return fs


deinitializeData :: FS.FileSystem -> IO ()
deinitializeData fs
  = do
    FS.closeFileSystem fs
