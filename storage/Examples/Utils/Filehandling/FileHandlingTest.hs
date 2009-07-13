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

import           Data.Binary
import qualified Data.ByteString.Lazy as B

import           Holumbus.Common.FileHandling


version :: String
version = "FileHandlingTest 0.1"


text1 :: String
text1 = "Hello"

text2 :: String
text2 = "World"

bin1 :: B.ByteString
bin1 = encode "Hello"

bin2 :: B.ByteString
bin2 = encode "World"

list1 :: [String]
list1 = ["Hello"]

list2 :: [String]
list2 = ["World"]


textfileName :: String
textfileName = "test.txt"

binfileName :: String
binfileName = "test.bin"

listfileName :: String
listfileName = "test.lst"

compareResults :: (Show a, Eq a) => a -> a -> IO ()
compareResults v1 v2
  = do
    putStrLn "comparing"
    putStrLn $ "v1: " ++ show v1
    putStrLn $ "v2: " ++ show v2
    putStrLn $ "equal: " ++ show (v1 == v2) 

main :: IO ()
main
  = do
    putStrLn version
    
    putStrLn ""
    putStrLn "textfileTest"
    writeToTextFile textfileName text1
    t1 <- readFromTextFile textfileName
    compareResults text1 t1
    appendToTextFile textfileName text2
    t2 <- readFromTextFile textfileName
    compareResults (text1 ++ text2) t2
    
    putStrLn ""
    putStrLn "binfileTest"
    writeToBinFile binfileName bin1
    b1 <- readFromBinFile binfileName
    compareResults bin1 b1
    appendToBinFile binfileName bin2
    b2 <- readFromBinFile binfileName
    compareResults (B.append bin1 bin2) b2
    
    putStrLn ""
    putStrLn "textfileTest"
    writeToListFile listfileName list1
    l1 <- readFromListFile listfileName
    compareResults list1 l1
    appendToListFile listfileName list2
    l2 <- readFromListFile listfileName
    compareResults (list1 ++ list2) l2
    