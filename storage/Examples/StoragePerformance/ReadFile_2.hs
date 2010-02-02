module Main where

import Data.Time.Clock.POSIX
import GHC.Int
import Data.Binary
import Data.Maybe

import Holumbus.FileSystem.FileSystem
import Holumbus.Network.PortRegistry.PortRegistryPort


import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
  setPortRegistry p      

  -- make filesystem
  fs <- mkFileSystemClient defaultFSClientConfig
  
  -- get time
  t <- getPOSIXTime
  t `seq` putStrLn . show $ t

  -- copy to fs
  binList <- getMultiFileContent ( map (\i -> "File_" ++show i) [1..100] ) fs

  -- get time 2
  t' <- getPOSIXTime
  t' `seq` putStrLn . show $ t'

  let duration = t' - t;
      bytes = fromIntegral . B.length .  encode $ binList;
      kbytes = bytes / 1024;
      mbytes = kbytes / 1024;
      bytesPerSecond  = bytes / duration
      kbytesPerSecond = kbytes / duration
      mbytesPerSecond = mbytes / duration

  putStrLn $ "Duration    : " ++ show duration
  putStrLn $ "Bytes copied: " ++ show bytes
  putStrLn $ "Bytes / s   : " ++ show bytesPerSecond
  putStrLn $ "KBytes / s  : " ++ show kbytesPerSecond
  putStrLn $ "MBytes / s  : " ++ show mbytesPerSecond
