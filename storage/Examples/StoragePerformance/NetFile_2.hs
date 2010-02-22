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
  putStrLn . show $ t

  -- copy to fs
  let filenames = map (\i -> "File_" ++ show i) [1..1000];
      files     = zip filenames binLists
  createFiles files fs

  -- get time 2
  t' <- getPOSIXTime
  putStrLn . show $ t'

  let duration = t' - t
  putStrLn $ "Duration    : " ++ show duration

binLists ::[B.ByteString]
binLists = map encode empty

empty :: [[Int32]]
empty = replicate 1000 []
