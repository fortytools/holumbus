module Main where

import Data.Time.Clock.POSIX
import GHC.Int
import Data.Binary

import Holumbus.FileSystem.FileSystem
import Holumbus.Network.PortRegistry.PortRegistryPort


import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  -- get time
  t0 <- getPOSIXTime
  t0 `seq` putStrLn . show $ t0

  p <- newPortRegistryFromXmlFile "/tmp/registry.xml"
  setPortRegistry p      

  -- make filesystem
  fs <- mkFileSystemClient defaultFSClientConfig
  
  -- fully evaluate the binList
  B.writeFile "/dev/null" binList

  -- get time
  t <- getPOSIXTime
  t `seq` putStrLn . show $ t

  -- copy to fs
  createFile "File" binList fs

  -- get time 2
  t' <- getPOSIXTime
  t' `seq` putStrLn . show $ t'

  let duration = t' - t;
      bytes = fromIntegral $ B.length binList;
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

binList ::B.ByteString
binList = encode list

list :: [[Int64]]
list = [[1..10^6] | _<-[1..10]]
