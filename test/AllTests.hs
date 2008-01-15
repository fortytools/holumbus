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
import Test.HUnit

import qualified StrMapTest as StrMap
import qualified BiMapTest as BiMap
import qualified ParserTest as Parser
import qualified FuzzyTest as Fuzzy
import qualified PickleTest as Pickle

allTests :: Test
allTests = TestList
           [ StrMap.allTests
           , BiMap.allTests
           , Parser.allTests
           , Fuzzy.allTests
           , Pickle.allTests
           ]

main :: IO ()
main = do
         c <- runTestTT allTests
         putStrLn $ show c
         let errs = errors c
             fails = failures c
         System.exitWith (codeGet errs fails)

codeGet :: Int -> Int -> ExitCode
codeGet errs fails | fails > 0 = ExitFailure 2
                   | errs > 0  = ExitFailure 1
                   | otherwise = ExitSuccess