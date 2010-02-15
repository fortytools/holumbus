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
  
  -- get time
  t <- getPOSIXTime
  t `seq` putStrLn . show $ t

  -- copy to fs
  mapM_ (\(i,bin) -> createFile ("File"++show i) bin fs) $ zip [0..] binLists

  -- get time 2
  t' <- getPOSIXTime
  t' `seq` putStrLn . show $ t'

  let duration = t' - t;
  putStrLn $ "Duration    : " ++ show duration

binLists ::[B.ByteString]
binLists = map encode list

list :: [[Int32]]
list = [[] | _<-[1..1000]]
