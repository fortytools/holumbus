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
  mapM_ (B.writeFile "/dev/null") binLists

  -- get time
  t <- getPOSIXTime
  t `seq` putStrLn . show $ t

  -- copy to fs
  let filenames = map (\i -> "File_" ++ show i) [1..100];
      files     = zip filenames binLists
  createFiles files fs

  -- get time 2
  t' <- getPOSIXTime
  t' `seq` putStrLn . show $ t'

  let duration = t' - t;
      bytes = fromIntegral . B.length . encode $ files;
      kbytes = bytes / 1024;
      mbytes = kbytes / 1024;
      bytesPerSecond  = bytes / duration
      mbytesPerSecond = mbytes / duration

  putStrLn $ "Duration    : " ++ show duration
  putStrLn $ "Bytes copied: " ++ show bytes
  putStrLn $ "MBytes copied: " ++ show mbytes
  putStrLn $ "Bytes / s   : " ++ show bytesPerSecond
  putStrLn $ "MBytes / s  : " ++ show mbytesPerSecond

binLists ::[B.ByteString]
binLists = map encode list

list :: [[Int64]]
list = [[1..10^6] | _<-[1..100]]
