-- ----------------------------------------------------------------------------

{- |
  Module     : Main
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module just bundles all Holumbus tests.

-}

-- ----------------------------------------------------------------------------

module Main where

import System
import System.IO
import Test.HUnit
import Test.QuickCheck.Batch

import qualified StrMapTest as StrMap
import qualified DiffListTest as DiffList
import qualified ParserTest as Parser
import qualified FuzzyTest as Fuzzy
import qualified PickleTest as Pickle
import qualified BinaryTest as Binary
import qualified InvertedTest as Inverted
import qualified DocumentsTest as Documents
import qualified CrunchTest as Crunch
import qualified PrefixTreeTest as Prefix

allTests :: Test
allTests = TestList
           [ StrMap.allTests
           , DiffList.allTests
           , Parser.allTests
           , Fuzzy.allTests
           , Pickle.allTests
           , Binary.allTests
           , Inverted.allTests
           , Documents.allTests
           , Crunch.allTests
           , Prefix.allTests
           ]

allProperties :: [(String, [TestOptions -> IO TestResult])]
allProperties = [ Parser.allProperties
                , Crunch.allProperties
                , DiffList.allProperties
                , Documents.allProperties
                , Inverted.allProperties
                , StrMap.allProperties
                , Prefix.allProperties
                ]

testOptions :: TestOptions
testOptions = TestOptions 100 300 False

runUnitTests :: IO Bool
runUnitTests = do
               putStrLn "=== Running Unit tests ==="
               (c, _) <- runTestText (putTextToHandle stderr False) allTests
               let errs = errors c
                   fails = failures c
               return (errs == 0 && fails == 0)

runQuickCheckTests :: IO Bool
runQuickCheckTests = do
                     putStrLn "=== Runnig QuickCheck tests ==="
                     quickCheckList allProperties
                     return True
                     where
                       quickCheckList [] = return ()
                       quickCheckList ((d, t):ts) = do
                                                    runTests d testOptions t
                                                    quickCheckList ts

runPrefixTests :: IO Bool
runPrefixTests = do
               putStrLn "=== Running Prefix unit tests ==="
               (c, _) <- runTestText (putTextToHandle stderr False) Prefix.allTests
               let errs = errors c
                   fails = failures c
               return (errs == 0 && fails == 0)


main :: IO ()
main = do
       argv <- getArgs
       if null argv || (not ("-u" `elem` argv) && not ("-q" `elem` argv) && not ("-p" `elem` argv)) then usage else return ()
       ut <- if "-u" `elem` argv then runUnitTests else return True
       qt <- if "-q" `elem` argv then runQuickCheckTests else return True
       pt <- if "-p" `elem` argv then runPrefixTests else return True
       if ut && qt && pt then return () else exitFailure

usage :: IO ()
usage = do
        putStrLn "AllTests - Execute all Holumbus tests\n"
        putStrLn "Usage: AllTests [OPTIONS]"
        putStrLn "  -u  Run HUnit tests"
        putStrLn "  -q  Run QuickCheck tests"
        putStrLn "  -p  Run prefix tree tests\n"
